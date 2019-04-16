open Globals

type define = {
	mutable values : (string,string) PMap.t;
	mutable defines_signature : string option;
}

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
	| DumpDependencies
	| DumpIgnoreVarIds
	| DynamicInterfaceClosures
	| EraseGenerics
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
	| HxcppApiLevel
	| HxcppGcGenerational
	| HxcppDebugger
	| HxcppSmartStings
	| IncludePrefix
	| Interp
	| JavaVer
	| JsClassic
	| JsEs
	| JsUnflatten
	| JsSourceMap
	| JsEnumsAsArrays
	| SourceMap
	| KeepOldOutput
	| LoopUnrollMaxCost
	| LuaJit
	| LuaVanilla
	| LuaVer
	| Macro
	| MacroTimes
	| NekoSource
	| NekoV1
	| NetworkSandbox
	| NetVer
	| NetTarget
	| NoCompilation
	| NoCOpt
	| NoDeprecationWarnings
	| NoFlashOverride
	| NoDebug
	| NoInline
	| NoOpt
	| NoRoot
	| NoSwfCompress
	| NoTraces
	| Objc
	| OldConstructorInline
	| OldErrorFormat
	| PhpLib
	| PhpFront
	| PhpPrefix
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
	| NoMacroCache
	| Last (* must be last *)

type define_parameter =
	| HasParam of string
	| Platform of platform
	| Platforms of platform list

let infos = function
	| AbsolutePath -> "absolute_path",("Print absolute file path in trace output",[])
	| AdvancedTelemetry -> "advanced-telemetry",("Allow the SWF to be measured with Monocle tool",[Platform Flash])
	| AnnotateSource -> "annotate_source",("Add additional comments to generated source code",[Platform Cpp])
	(* | Analyzer -> "analyzer",("Use static analyzer for optimization (experimental)") *)
	| As3 -> "as3",("Defined when outputting flash9 as3 source code",[])
	| CheckXmlProxy -> "check_xml_proxy",("Check the used fields of the xml proxy",[])
	| CoreApi -> "core_api",("Defined in the core api context",[])
	| CoreApiSerialize -> "core_api_serialize",("Mark some generated core api classes with the Serializable attribute on C#",[Platform Cs])
	| Cppia -> "cppia",("Generate cpp instruction assembly",[])
	| NoCppiaAst -> "nocppiaast",("Use legacy cppia generation",[])
	| Dce -> "dce",("<mode:std|full|no> Set the dead code elimination mode (default std)",[])
	| DceDebug -> "dce_debug",("Show DCE log",[])
	| Debug -> "debug",("Activated when compiling with -debug",[])
	| DisableUnicodeStrings -> "disable_unicode_strings",("Disable unicode support in String type on some platforms",[Platform Cpp])
	| Display -> "display",("Activated during completion",[])
	| DisplayStdin -> "display_stdin",("Read the contents of a file specified in --display from standard input",[])
	| DllExport -> "dll_export",("GenCPP experimental linking",[Platform Cpp])
	| DllImport -> "dll_import",("Handle Haxe-generated .NET dll imports",[Platform Cs])
	| DocGen -> "doc_gen",("Do not perform any removal/change in order to correctly generate documentation",[])
	| Dump -> "dump",("<mode:pretty|record|position|legacy> Dump typed AST in dump subdirectory using specified mode or non-prettified default",[])
	| DumpDependencies -> "dump_dependencies",("Dump the classes dependencies in a dump subdirectory",[])
	| DumpIgnoreVarIds -> "dump_ignore_var_ids",("Remove variable IDs from non-pretty dumps (helps with diff)",[])
	| DynamicInterfaceClosures -> "dynamic_interface_closures",("Use slow path for interface closures to save space",[Platform Cpp])
	| EraseGenerics -> "erase_generics",("Erase generic classes on C#",[Platform Cs])
	| EvalDebugger -> "eval_debugger",("Support debugger in macro/interp mode. Allows host:port value to open a socket. Implies eval_stack.",[])
	| EvalStack -> "eval_stack",("Record stack information in macro/interp mode",[])
	| EvalTimes -> "eval_times",("Record per-method execution times in macro/interp mode. Implies eval_stack.",[])
	| FastCast -> "fast_cast",("Enables an experimental casts cleanup on C# and Java",[Platforms [Cs;Java]])
	| Fdb -> "fdb",("Enable full flash debug infos for FDB interactive debugging",[Platform Flash])
	| FileExtension -> "file_extension",("Output filename extension for cpp source code",[Platform Cpp])
	| FlashStrict -> "flash_strict",("More strict typing for flash target",[Platform Flash])
	| FlashUseStage -> "flash_use_stage",("Keep the SWF library initial stage",[Platform Flash])
	(* force_lib_check is only here as a debug facility - compiler checking allows errors to be found more easily *)
	| ForceLibCheck -> "force_lib_check",("Force the compiler to check -net-lib and -java-lib added classes (internal)",[Platforms [Cs;Java]])
	| ForceNativeProperty -> "force_native_property",("Tag all properties with :nativeProperty metadata for 3.1 compatibility",[Platform Cpp])
	| GencommonDebug -> "gencommon_debug",("GenCommon internal",[Platforms [Cs;Java]])
	| Haxe3Compat -> "haxe3compat", ("Gives warnings about transition from Haxe 3.x to Haxe 4.0",[])
	| HaxeBoot -> "haxe_boot",("Given the name 'haxe' to the flash boot class instead of a generated name",[Platform Flash])
	| HaxeVer -> "haxe_ver",("The current Haxe version value",[])
	| HxcppApiLevel -> "hxcpp_api_level",("Provided to allow compatibility between hxcpp versions",[Platform Cpp])
	| HxcppGcGenerational -> "HXCPP_GC_GENERATIONAL",("Experimental Garbage Collector",[Platform Cpp])
	| HxcppDebugger -> "HXCPP_DEBUGGER",("Include additional information for HXCPP_DEBUGGER",[Platform Cpp])
	| HxcppSmartStings -> "hxcpp_smart_strings",("Use wide strings in hxcpp (Turned on by default unless `-D disable_unicode_strings` is specified)",[Platform Cpp])
	| IncludePrefix -> "include_prefix",("prepend path to generated include files",[Platform Cpp])
	| Interp -> "interp",("The code is compiled to be run with --interp",[])
	| JavaVer -> "java_ver",("<version:5-7> Sets the Java version to be targeted",[Platform Java])
	| JsClassic -> "js_classic",("Don't use a function wrapper and strict mode in JS output",[Platform Js])
	| JsEs -> "js_es",("Generate JS compliant with given ES standard version (default 5)",[Platform Js; HasParam "version number"])
	| JsEnumsAsArrays -> "js_enums_as_arrays",("Generate enum representation as array instead of as object",[Platform Js])
	| JsUnflatten -> "js_unflatten",("Generate nested objects for packages and types",[Platform Js])
	| JsSourceMap -> "js_source_map",("Generate JavaScript source map even in non-debug mode",[Platform Js])
	| SourceMap -> "source_map",("Generate source map for compiled files (Currently supported for php only)",[Platform Php])
	| KeepOldOutput -> "keep_old_output",("Keep old source files in the output directory (for C#/Java)",[Platforms [Cs;Java]])
	| LoopUnrollMaxCost -> "loop_unroll_max_cost",("Maximum cost (number of expressions * iterations) before loop unrolling is canceled (default 250)",[])
	| LuaJit -> "lua_jit",("Enable the jit compiler for lua (version 5.2 only)",[Platform Lua])
	| LuaVanilla -> "lua_vanilla",("Generate code lacking compiled extern lib support (e.g. utf8)",[Platform Lua])
	| LuaVer -> "lua_ver",("The lua version to target",[Platform Lua])
	| Macro -> "macro",("Defined when code is compiled in the macro context",[])
	| MacroTimes -> "macro_times",("Display per-macro timing when used with --times",[])
	| NetVer -> "net_ver",("<version:20-45> Sets the .NET version to be targeted",[Platform Cs])
	| NetTarget -> "net_target",("<name> Sets the .NET target. Defaults to \"net\". xbox, micro (Micro Framework), compact (Compact Framework) are some valid values",[Platform Cs])
	| NekoSource -> "neko_source",("Output neko source instead of bytecode",[Platform Neko])
	| NekoV1 -> "neko_v1",("Keep Neko 1.x compatibility",[Platform Neko])
	| NetworkSandbox -> "network-sandbox",("Use local network sandbox instead of local file access one",[Platform Flash])
	| NoCompilation -> "no-compilation",("Disable final compilation",[Platforms [Cs;Java;Cpp;Hl]])
	| NoCOpt -> "no_copt",("Disable completion optimization (for debug purposes)",[])
	| NoDebug -> "no_debug",("Remove all debug macros from cpp output",[])
	| NoDeprecationWarnings -> "no-deprecation-warnings",("Do not warn if fields annotated with @:deprecated are used",[])
	| NoFlashOverride -> "no-flash-override",("Change overrides on some basic classes into HX suffixed methods, flash only",[Platform Flash])
	| NoOpt -> "no_opt",("Disable optimizations",[])
	| NoInline -> "no_inline",("Disable inlining",[])
	| NoRoot -> "no_root",("Generate top-level types into haxe.root namespace",[Platform Cs])
	| NoMacroCache -> "no_macro_cache",("Disable macro context caching",[])
	| NoSwfCompress -> "no_swf_compress",("Disable SWF output compression",[Platform Flash])
	| NoTraces -> "no_traces",("Disable all trace calls",[])
	| Objc -> "objc",("Sets the hxcpp output to objective-c++ classes. Must be defined for interop",[Platform Cpp])
	| OldConstructorInline -> "old-constructor-inline",("Use old constructor inlining logic (from haxe 3.4.2) instead of the reworked version.",[])
	| OldErrorFormat -> "old-error-format",("Use Haxe 3.x zero-based column error messages instead of new one-based format.",[])
	| PhpPrefix -> "php_prefix",("Root namespace for generated php classes. E.g. if compiled with`-D php-prefix=some.sub`, then all classes will be generated in `\\some\\sub` namespace.",[Platform Php])
	| PhpLib -> "php_lib",("Select the name for the php lib folder.",[Platform Php])
	| PhpFront -> "php_front",("Select the name for the php front file (by default: `index.php`).", [Platform Php])
	| PythonVersion -> "python_version",("The python version to target (default 3.3)",[Platform Python])
	| RealPosition -> "real_position",("Disables Haxe source mapping when targetting C#, removes position comments in Java and Php output",[Platforms [Cs;Java;Php]])
	| ReplaceFiles -> "replace_files",("GenCommon internal",[Platforms [Java;Cs]])
	| Scriptable -> "scriptable",("GenCPP internal",[Platform Cpp])
	| ShallowExpose -> "shallow-expose",("Expose types to surrounding scope of Haxe generated closure without writing to window object",[Platform Js])
	| SourceHeader -> "source-header",("Print value as comment on top of generated files, use '' value to disable",[])
	| SourceMapContent -> "source-map-content",("Include the hx sources as part of the JS source map",[Platform Js])
	| Static -> "static",("Defined if the current target is static",[])
	| Swc -> "swc",("Output a SWC instead of a SWF",[Platform Flash])
	| SwfCompressLevel -> "swf_compress_level",("<level:1-9> Set the amount of compression for the SWF output",[Platform Flash])
	| SwfDebugPassword -> "swf_debug_password",("Set a password for debugging",[Platform Flash])
	| SwfDirectBlit -> "swf_direct_blit",("Use hardware acceleration to blit graphics",[Platform Flash])
	| SwfGpu -> "swf_gpu",("Use GPU compositing features when drawing graphics",[Platform Flash])
	| SwfMetadata -> "swf_metadata",("<file> Include contents of <file> as metadata in the swf",[Platform Flash])
	| SwfPreloaderFrame -> "swf_preloader_frame",("Insert empty first frame in swf",[Platform Flash])
	| SwfProtected -> "swf_protected",("Compile Haxe private as protected in the SWF instead of public",[Platform Flash])
	| SwfScriptTimeout -> "swf_script_timeout",("Maximum ActionScript processing time before script stuck dialog box displays (in seconds)",[Platform Flash])
	| SwfUseDoAbc -> "swf_use_doabc",("Use DoAbc swf-tag instead of DoAbcDefine",[Platform Flash])
	| Sys -> "sys",("Defined for all system platforms",[])
	| Unsafe -> "unsafe",("Allow unsafe code when targeting C#",[Platform Cs])
	| UseNekoc -> "use_nekoc",("Use nekoc compiler instead of internal one",[Platform Neko])
	| Utf16 -> "utf16",("Defined for all platforms that have utf16 encoding with ucs2 api",[])
	| Vcproj -> "vcproj",("GenCPP internal",[Platform Cpp])
	| WarnVarShadowing -> "warn_var_shadowing",("Warn about shadowing variable declarations",[])
	| Last -> assert false

let get_documentation_list() =
	let m = ref 0 in
	let rec loop i =
		let d = Obj.magic i in
		if d <> Last then begin
			let t, (doc,flags) = infos d in
			let pfs = ref [] in
			List.iter (function
			| HasParam s -> () (* TODO *)
			| Platform p -> pfs := p :: !pfs;
			| Platforms pl -> pfs := pl @ !pfs;
			) flags;
			let pfs = platform_list_help (List.rev !pfs) in
			if String.length t > !m then m := String.length t;
			((String.concat "-" (ExtString.String.nsplit t "_")),doc ^ pfs) :: (loop (i + 1))
		end else
			[]
	in
	let all = List.sort (fun (s1,_) (s2,_) -> String.compare s1 s2) (loop 0) in
	all,!m

let raw_defined ctx v =
	PMap.mem v ctx.values

let defined ctx v =
	raw_defined ctx (fst (infos v))

let raw_defined_value ctx k =
	PMap.find k ctx.values

let defined_value ctx v =
	raw_defined_value ctx (fst (infos v))

let defined_value_safe ?default ctx v =
	try defined_value ctx v
	with Not_found -> match default with Some s -> s | None -> ""

let raw_define_value ctx k v =
	ctx.values <- PMap.add k v ctx.values;
	let k = String.concat "_" (ExtString.String.nsplit k "-") in
	ctx.values <- PMap.add k v ctx.values;
	ctx.defines_signature <- None

let raw_define ctx v =
	let k,v = try ExtString.String.split v "=" with _ -> v,"1" in
	raw_define_value ctx k v

let define_value ctx k v =
	raw_define ctx (fst (infos k) ^ "=" ^ v)

let define ctx v =
	raw_define ctx (fst (infos v))

let get_signature def =
	match def.defines_signature with
	| Some s -> s
	| None ->
		let defines = PMap.foldi (fun k v acc ->
			(* don't make much difference between these special compilation flags *)
			match String.concat "_" (ExtString.String.nsplit k "-") with
			(* If we add something here that might be used in conditional compilation it should be added to
			   Parser.parse_macro_ident as well (issue #5682).
			   Note that we should removed flags like use_rtti_doc here.
			*)
			| "display" | "use_rtti_doc" | "macro_times" | "display_details" | "no_copt" | "display_stdin"
			| "dump" | "dump_dependencies" | "dump_ignore_var_ids" -> acc
			| _ -> (k ^ "=" ^ v) :: acc
		) def.values [] in
		let str = String.concat "@" (List.sort compare defines) in
		let s = Digest.string str in
		def.defines_signature <- Some s;
		s

let is_haxe3_compat def = raw_defined def "hx3compat"
