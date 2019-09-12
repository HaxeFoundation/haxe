(* This file is auto-generated using prebuild from files in src-json *)
(* Do not edit manually! *)

open Globals

type define_parameter =
	| HasParam of string
	| Platforms of platform list
	| Link of string

type strict_defined =
	| AbsolutePath
	| AdvancedTelemetry
	| AnnotateSource
	| As3
	| CheckXmlProxy
	| CoreApi
	| CoreApiSerialize
	| Cppia
	| CsVer
	| NoCppiaAst
	| Dce
	| DceDebug
	| Debug
	| DisableUnicodeStrings
	| Display
	| DisplayStdin
	| DllExport
	| DllImport
	| DocGen
	| Dump
	| DumpPath
	| DumpDependencies
	| DumpIgnoreVarIds
	| DynamicInterfaceClosures
	| EraseGenerics
	| EvalCallStackDepth
	| EvalDebugger
	| EvalStack
	| EvalTimes
	| FastCast
	| Fdb
	| FileExtension
	| FlashStrict
	| FlashUseStage
	| ForceLibCheck
	| ForceNativeProperty
	| GencommonDebug
	| Haxe3Compat
	| HaxeBoot
	| HaxeVer
	| Haxe
	| HxcppApiLevel
	| HxcppGcGenerational
	| HxcppDebugger
	| HxcppSmartStings
	| IncludePrefix
	| Interp
	| JavaVer
	| JsClassic
	| JsEs
	| JsEnumsAsArrays
	| JsUnflatten
	| JsSourceMap
	| SourceMap
	| Jvm
	| KeepOldOutput
	| LoopUnrollMaxCost
	| LuaJit
	| LuaVanilla
	| LuaVer
	| Macro
	| MacroTimes
	| NetVer
	| NetcoreVer
	| NetTarget
	| NekoSource
	| NekoV1
	| NetworkSandbox
	| NoCompilation
	| NoCOpt
	| NoDebug
	| NoDeprecationWarnings
	| NoFlashOverride
	| NoOpt
	| NoInline
	| KeepInlinePositions
	| NoRoot
	| NoMacroCache
	| NoSwfCompress
	| NoTraces
	| Objc
	| OldConstructorInline
	| OldErrorFormat
	| PhpPrefix
	| PhpLib
	| PhpFront
	| PythonVersion
	| RealPosition
	| ReplaceFiles
	| Scriptable
	| ShallowExpose
	| SourceHeader
	| SourceMapContent
	| Static
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
	| Utf16
	| Vcproj
	| WarnVarShadowing
	| Last

let infos = function
	| AbsolutePath -> "absolute_path",("Print absolute file path in trace output.",[])
	| AdvancedTelemetry -> "advanced-telemetry",("Allow the SWF to be measured with Monocle tool.",[Platforms [Flash]])
	| AnnotateSource -> "annotate_source",("Add additional comments to generated source code.",[Platforms [Cpp]])
	| As3 -> "as3",("Defined when outputting flash9 as3 source code.",[])
	| CheckXmlProxy -> "check_xml_proxy",("Check the used fields of the XML proxy.",[])
	| CoreApi -> "core_api",("Defined in the core API context.",[])
	| CoreApiSerialize -> "core_api_serialize",("Mark some generated core API classes with the `Serializable` attribute on C#.",[Platforms [Cs]])
	| Cppia -> "cppia",("Generate cpp instruction assembly.",[])
	| CsVer -> "cs_ver",("The C# version to target.",[Platforms [Cs]])
	| NoCppiaAst -> "nocppiaast",("Use legacy cppia generation.",[])
	| Dce -> "dce",("Set the dead code elimination mode. (default: std)",[HasParam "mode: std | full | no"; Link "https://haxe.org/manual/cr-dce.html"])
	| DceDebug -> "dce_debug",("Show DCE log.",[Link "https://haxe.org/manual/cr-dce.html"])
	| Debug -> "debug",("Activated when compiling with -debug.",[])
	| DisableUnicodeStrings -> "disable_unicode_strings",("Disable Unicode support in `String` type.",[Platforms [Cpp]])
	| Display -> "display",("Activated during completion.",[Link "https://haxe.org/manual/cr-completion.html"])
	| DisplayStdin -> "display_stdin",("Read the contents of a file specified in `--display` from standard input.",[])
	| DllExport -> "dll_export",("GenCPP experimental linking.",[Platforms [Cpp]])
	| DllImport -> "dll_import",("Handle Haxe-generated .NET DLL imports.",[Platforms [Cs]])
	| DocGen -> "doc_gen",("Do not perform any removal/change in order to correctly generate documentation.",[])
	| Dump -> "dump",("Dump typed AST in dump subdirectory using specified mode or non-prettified default.",[HasParam "mode: pretty | record | position | legacy"])
	| DumpPath -> "dump_path",("Path to generate dumps to (default: \"dump\").",[])
	| DumpDependencies -> "dump_dependencies",("Dump the classes dependencies in a dump subdirectory.",[])
	| DumpIgnoreVarIds -> "dump_ignore_var_ids",("Remove variable IDs from non-pretty dumps (helps with diff).",[])
	| DynamicInterfaceClosures -> "dynamic_interface_closures",("Use slow path for interface closures to save space.",[Platforms [Cpp]])
	| EraseGenerics -> "erase_generics",("Erase generic classes on C#.",[Platforms [Cs]])
	| EvalCallStackDepth -> "eval_call_stack_depth",("Set maximum call stack depth for eval. (default: 1000)",[Platforms [Eval]; HasParam "depth"])
	| EvalDebugger -> "eval_debugger",("Support debugger in macro/interp mode. Allows `host:port` value to open a socket. Implies eval_stack.",[Platforms [Eval]])
	| EvalStack -> "eval_stack",("Record stack information in macro/interp mode.",[Platforms [Eval]])
	| EvalTimes -> "eval_times",("Record per-method execution times in macro/interp mode. Implies eval_stack.",[Platforms [Eval]])
	| FastCast -> "fast_cast",("Enables an experimental casts cleanup on C# and Java.",[Platforms [Cs;Java]])
	| Fdb -> "fdb",("Enable full flash debug infos for FDB interactive debugging.",[Platforms [Flash]])
	| FileExtension -> "file_extension",("Output filename extension for cpp source code.",[Platforms [Cpp]])
	| FlashStrict -> "flash_strict",("More strict typing for flash target.",[Platforms [Flash]])
	| FlashUseStage -> "flash_use_stage",("Keep the SWF library initial stage.",[Platforms [Flash]])
	| ForceLibCheck -> "force_lib_check",("Force the compiler to check `--net-lib` and `\226\128\147-java-lib` added classes (internal).",[Platforms [Cs;Java]])
	| ForceNativeProperty -> "force_native_property",("Tag all properties with `:nativeProperty` metadata for 3.1 compatibility.",[Platforms [Cpp]])
	| GencommonDebug -> "gencommon_debug",("GenCommon internal.",[Platforms [Cs;Java]])
	| Haxe3Compat -> "haxe3compat",("Gives warnings about transition from Haxe 3.x to Haxe 4.0.",[])
	| HaxeBoot -> "haxe_boot",("Give the name 'haxe' to the flash boot class instead of a generated name.",[Platforms [Flash]])
	| HaxeVer -> "haxe_ver",("The current Haxe version value as decimal number. E.g. 3.407 for 3.4.7.",[])
	| Haxe -> "haxe",("The current Haxe version value in SemVer format.",[])
	| HxcppApiLevel -> "hxcpp_api_level",("Provided to allow compatibility between hxcpp versions.",[Platforms [Cpp]])
	| HxcppGcGenerational -> "HXCPP_GC_GENERATIONAL",("Experimental Garbage Collector.",[Platforms [Cpp]])
	| HxcppDebugger -> "HXCPP_DEBUGGER",("Include additional information for hxcpp_debugger.",[Platforms [Cpp]])
	| HxcppSmartStings -> "hxcpp_smart_strings",("Use wide strings in hxcpp. (Turned on by default unless `-D disable_unicode_strings` is specified.)",[Platforms [Cpp]])
	| IncludePrefix -> "include_prefix",("Prepend path to generated include files.",[Platforms [Cpp]])
	| Interp -> "interp",("The code is compiled to be run with `--interp`.",[])
	| JavaVer -> "java_ver",("Sets the Java version to be targeted.",[Platforms [Java]; HasParam "version: 5-7"])
	| JsClassic -> "js_classic",("Don't use a function wrapper and strict mode in JS output.",[Platforms [Js]])
	| JsEs -> "js_es",("Generate JS compliant with given ES standard version. (default: 5)",[Platforms [Js]; HasParam "version number"; Link "https://haxe.org/manual/target-javascript-es6.html"])
	| JsEnumsAsArrays -> "js_enums_as_arrays",("Generate enum representation as array instead of as object.",[Platforms [Js]])
	| JsUnflatten -> "js_unflatten",("Generate nested objects for packages and types.",[Platforms [Js]])
	| JsSourceMap -> "js_source_map",("Generate JavaScript source map even in non-debug mode. Deprecated in favor of `-D source_map`.",[Platforms [Js]])
	| SourceMap -> "source_map",("Generate source map for compiled files. Note: for JavaScript prefer",[Platforms [Php;Js]])
	| Jvm -> "jvm",("Generate jvm directly.",[Platforms [Java]])
	| KeepOldOutput -> "keep_old_output",("Keep old source files in the output directory.",[Platforms [Cs;Java]])
	| LoopUnrollMaxCost -> "loop_unroll_max_cost",("Maximum cost (number of expressions * iterations) before loop unrolling is canceled. (default: 250)",[HasParam "cost"])
	| LuaJit -> "lua_jit",("Enable the jit compiler for lua (version 5.2 only).",[Platforms [Lua]])
	| LuaVanilla -> "lua_vanilla",("Generate code lacking compiled extern lib support (e.g. utf8).",[Platforms [Lua]])
	| LuaVer -> "lua_ver",("The lua version to target.",[Platforms [Lua]; HasParam "version"])
	| Macro -> "macro",("Defined when code is compiled in the macro context.",[Link "https://haxe.org/manual/macro.html"])
	| MacroTimes -> "macro_times",("Display per-macro timing when used with `--times`.",[])
	| NetVer -> "net_ver",("Sets the .NET version to be targeted.",[Platforms [Cs]; HasParam "version: 20-45"])
	| NetcoreVer -> "netcore_ver",("Sets the .NET core version to be targeted",[Platforms [Cs]; HasParam "version: x.x.x"])
	| NetTarget -> "net_target",("Sets the .NET target. `netcore` (.NET core), `xbox`, `micro` (Micro Framework), `compact` (Compact Framework) are some valid values. (default: `net`)",[Platforms [Cs]; HasParam "name"])
	| NekoSource -> "neko_source",("Output neko source instead of bytecode.",[Platforms [Neko]])
	| NekoV1 -> "neko_v1",("Keep Neko 1.x compatibility.",[Platforms [Neko]])
	| NetworkSandbox -> "network-sandbox",("Use local network sandbox instead of local file access one.",[Platforms [Flash]])
	| NoCompilation -> "no-compilation",("Disable final compilation.",[Platforms [Cs;Java;Cpp;Hl]])
	| NoCOpt -> "no_copt",("Disable completion optimization (for debug purposes).",[])
	| NoDebug -> "no_debug",("Remove all debug macros from cpp output.",[])
	| NoDeprecationWarnings -> "no-deprecation-warnings",("Do not warn if fields annotated with `@:deprecated` are used.",[])
	| NoFlashOverride -> "no-flash-override",("Change overrides on some basic classes into HX suffixed methods",[Platforms [Flash]])
	| NoOpt -> "no_opt",("Disable optimizations.",[])
	| NoInline -> "no_inline",("Disable inlining.",[Link "https://haxe.org/manual/class-field-inline.html"])
	| KeepInlinePositions -> "keep_inline_positions",("Don't substitute positions of inlined expressions with the position of the place of inlining.",[Link "https://haxe.org/manual/class-field-inline.html"])
	| NoRoot -> "no_root",("Generate top-level types into the `haxe.root` namespace.",[Platforms [Cs]])
	| NoMacroCache -> "no_macro_cache",("Disable macro context caching.",[])
	| NoSwfCompress -> "no_swf_compress",("Disable SWF output compression.",[Platforms [Flash]])
	| NoTraces -> "no_traces",("Disable all trace calls.",[])
	| Objc -> "objc",("Sets the hxcpp output to Objective-C++ classes. Must be defined for interop.",[Platforms [Cpp]])
	| OldConstructorInline -> "old-constructor-inline",("Use old constructor inlining logic (from Haxe 3.4.2) instead of the reworked version.",[])
	| OldErrorFormat -> "old-error-format",("Use Haxe 3.x zero-based column error messages instead of new one-based format.",[])
	| PhpPrefix -> "php_prefix",("Root namespace for generated php classes. E.g. if compiled with`-D php-prefix=some.sub`, then all classes will be generated in `\\some\\sub` namespace.",[Platforms [Php]; HasParam "dot-separated namespace"])
	| PhpLib -> "php_lib",("Select the name for the php lib folder.",[Platforms [Php]; HasParam "folder name"])
	| PhpFront -> "php_front",("Select the name for the php front file. (default: `index.php`)",[Platforms [Php]; HasParam "filename"])
	| PythonVersion -> "python_version",("The python version to target. (default: 3.3)",[Platforms [Python]; HasParam "version"])
	| RealPosition -> "real_position",("Disables Haxe source mapping when targetting C#, removes position comments in Java and Php output.",[Platforms [Cs;Java;Php]])
	| ReplaceFiles -> "replace_files",("GenCommon internal.",[Platforms [Cs;Java]])
	| Scriptable -> "scriptable",("GenCPP internal.",[Platforms [Cpp]])
	| ShallowExpose -> "shallow-expose",("Expose types to surrounding scope of Haxe generated closure without writing to window object.",[Platforms [Js]])
	| SourceHeader -> "source-header",("Print value as comment on top of generated files, use '' value to disable.",[])
	| SourceMapContent -> "source-map-content",("Include the Haxe sources as part of the JS source map.",[Platforms [Js]])
	| Static -> "static",("Defined if the current target is static.",[])
	| Swc -> "swc",("Output a SWC instead of a SWF.",[Platforms [Flash]])
	| SwfCompressLevel -> "swf_compress_level",("Set the amount of compression for the SWF output.",[Platforms [Flash]; HasParam "level: 1-9"])
	| SwfDebugPassword -> "swf_debug_password",("Set a password for debugging.",[Platforms [Flash]; HasParam "password"])
	| SwfDirectBlit -> "swf_direct_blit",("Use hardware acceleration to blit graphics.",[Platforms [Flash]])
	| SwfGpu -> "swf_gpu",("Use GPU compositing features when drawing graphics.",[Platforms [Flash]])
	| SwfMetadata -> "swf_metadata",("Include contents of the given file as metadata in the SWF.",[Platforms [Flash]; HasParam "file"])
	| SwfPreloaderFrame -> "swf_preloader_frame",("Insert empty first frame in SWF.",[Platforms [Flash]])
	| SwfProtected -> "swf_protected",("Compile Haxe `private` as `protected` in the SWF instead of `public`.",[Platforms [Flash]])
	| SwfScriptTimeout -> "swf_script_timeout",("Maximum ActionScript processing time before script stuck dialog box displays.",[Platforms [Flash]; HasParam "time in seconds"])
	| SwfUseDoAbc -> "swf_use_doabc",("Use `DoAbc` SWF-tag instead of `DoAbcDefine`.",[Platforms [Flash]])
	| Sys -> "sys",("Defined for all system platforms.",[])
	| Unsafe -> "unsafe",("Allow unsafe code when targeting C#.",[Platforms [Cs]])
	| UseNekoc -> "use_nekoc",("Use `nekoc` compiler instead of the internal one.",[Platforms [Neko]])
	| Utf16 -> "utf16",("Defined for all platforms that use UTF-16 string encoding with UCS-2 API.",[])
	| Vcproj -> "vcproj",("GenCPP internal.",[Platforms [Cpp]])
	| WarnVarShadowing -> "warn_var_shadowing",("Warn about shadowing variable declarations.",[])
	| Last -> assert false
