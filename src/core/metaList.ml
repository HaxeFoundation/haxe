(* This file is auto-generated using prebuild from files in src-json *)
(* Do not edit manually! *)

open Globals

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
	| TVariable

type meta_parameter =
	| HasParam of string
	| Platforms of platform list
	| UsedOn of meta_usage list
	| UsedInternally
	| Link of string

type strict_meta =
	| Abi
	| Abstract
	| Access
	| Accessor
	| Allow
	| Analyzer
	| Annotation
	| ArrayAccess
	| Ast
	| AstSource
	| AutoBuild
	| Bind
	| Bitmap
	| BridgeProperties
	| Build
	| BuildXml
	| BypassAccessor
	| Callable
	| Class
	| ClassCode
	| Commutative
	| CompilerGenerated
	| Const
	| CoreApi
	| CoreType
	| CppFileCode
	| CppInclude
	| CppNamespaceCode
	| CsNative
	| Dce
	| Debug
	| Decl
	| DefParam
	| Delegate
	| Depend
	| Deprecated
	| DirectlyUsed
	| DisplayOverride
	| DynamicObject
	| Eager
	| Enum
	| EnumConstructorParam
	| Event
	| Exhaustive
	| Expose
	| Extern
	| File
	| FileXml
	| Final
	| Fixed
	| FlashProperty
	| FlatEnum
	| Font
	| ForLoopVariable
	| Forward
	| ForwardStatics
	| From
	| FunctionCode
	| FunctionTailCode
	| Generic
	| GenericBuild
	| GenericInstance
	| Getter
	| Hack
	| HasUntyped
	| HaxeGeneric
	| HeaderClassCode
	| HeaderCode
	| HeaderInclude
	| HeaderNamespaceCode
	| HlNative
	| HxGen
	| IfFeature
	| Impl
	| PythonImport
	| ImplicitCast
	| ImplicitReturn
	| Include
	| InitPackage
	| Inline
	| InlineConstructorArgument of int * int
	| Internal
	| IsVar
	| JavaCanonical
	| JavaNative
	| JsRequire
	| LuaRequire
	| LuaDotMethod
	| Keep
	| KeepInit
	| KeepSub
	| LibType
	| LoopLabel
	| Markup
	| Meta
	| Macro
	| MaybeUsed
	| MergeBlock
	| MultiReturn
	| MultiType
	| Native
	| NativeChildren
	| NativeGen
	| NativeGeneric
	| NativeProperty
	| NativeStaticExtension
	| NoCompletion
	| NoClosure
	| NoDebug
	| NoDoc
	| NoExpr
	| NoImportGlobal
	| NonVirtual
	| NoPackageRestrict
	| NoPrivateAccess
	| NoStack
	| NotNull
	| NoUsing
	| Ns
	| NullSafety
	| Objc
	| ObjcProtocol
	| Op
	| Optional
	| Overload
	| Persistent
	| PhpGlobal
	| PhpClassConst
	| PhpMagic
	| PhpNoConstructor
	| Pos
	| Public
	| PublicFields
	| Private
	| PrivateAccess
	| Protected
	| Property
	| Pure
	| ReadOnly
	| RealPath
	| Remove
	| Require
	| RequiresAssign
	| Resolve
	| Rtti
	| Runtime
	| RuntimeValue
	| Scalar
	| SelfCall
	| Semantics
	| Setter
	| SkipCtor
	| SkipReflection
	| Sound
	| SourceFile
	| StackOnly
	| StaticExtension
	| StoredTypedExpr
	| Strict
	| Struct
	| StructAccess
	| StructInit
	| SuppressWarnings
	| TemplatedCall
	| Throws
	| This
	| To
	| ToString
	| Transient
	| ValueUsed
	| Volatile
	| UnifyMinDynamic
	| Unreflective
	| Unsafe
	| Used
	| Using
	| Value
	| Void
	| Last
	| Dollar of string
	| Custom of string

let get_info = function
	| Abi -> ":abi",("Function ABI/calling convention.",[Platforms [Cpp]])
	| Abstract -> ":abstract",("Sets the underlying class implementation as `abstract`.",[Platforms [Java;Cs]])
	| Access -> ":access",("Forces private access to package, type or field.",[HasParam "Target path"; UsedOn [TClass;TClassField]; Link "https://haxe.org/manual/lf-access-control.html"])
	| Accessor -> ":accessor",("Used internally by DCE to mark property accessors.",[UsedOn [TClassField]; UsedInternally])
	| Allow -> ":allow",("Allows private access from package, type or field.",[HasParam "Target path"; Link "https://haxe.org/manual/lf-access-control.html"])
	| Analyzer -> ":analyzer",("Used to configure the static analyzer.",[])
	| Annotation -> ":annotation",("Annotation (`@interface`) definitions on `--java-lib` imports will be annotated with this metadata. Has no effect on types compiled by Haxe.",[Platforms [Java]; UsedOn [TClass]])
	| ArrayAccess -> ":arrayAccess",("Allows array access on an abstract.",[UsedOn [TAbstract;TAbstractField]; Link "https://haxe.org/manual/types-abstract-array-access.html"])
	| Ast -> ":ast",("Internally used to pass the AST source into the typed AST.",[UsedInternally])
	| AstSource -> ":astSource",("Filled by the compiler with the parsed expression of the field.",[UsedOn [TClassField]])
	| AutoBuild -> ":autoBuild",("Extends `@:build` metadata to all extending and implementing classes.",[HasParam "Build macro call"; UsedOn [TClass]; Link "https://haxe.org/manual/macro-auto-build.html"])
	| Bind -> ":bind",("Override SWF class declaration.",[Platforms [Flash]; UsedOn [TClass]])
	| Bitmap -> ":bitmap",("Embeds given bitmap data into the class (must extend `flash.display.BitmapData`).",[Platforms [Flash]; HasParam "Bitmap file path"; UsedOn [TClass]; Link "https://haxe.org/manual/target-flash-resources.html"])
	| BridgeProperties -> ":bridgeProperties",("Creates native property bridges for all Haxe properties in this class.",[Platforms [Cs]; UsedOn [TClass]])
	| Build -> ":build",("Builds a class or enum from a macro.",[HasParam "Build macro call"; UsedOn [TClass;TEnum]; Link "https://haxe.org/manual/macro-type-building.html"])
	| BuildXml -> ":buildXml",("Specify XML data to be injected into `Build.xml`.",[Platforms [Cpp]])
	| BypassAccessor -> ":bypassAccessor",("Do not call property accessor method and access the field directly.",[UsedOn [TExpr]; Link "https://haxe.org/manual/class-field-property.html"])
	| Callable -> ":callable",("Abstract forwards call to its underlying type.",[UsedOn [TAbstract]])
	| Class -> ":class",("Used internally to annotate an enum that will be generated as a class.",[Platforms [Java;Cs]; UsedOn [TEnum]; UsedInternally])
	| ClassCode -> ":classCode",("Used to inject platform-native code into a class.",[Platforms [Java;Cs]; UsedOn [TClass]])
	| Commutative -> ":commutative",("Declares an abstract operator as commutative.",[UsedOn [TAbstractField]; Link "https://haxe.org/manual/types-abstract-operator-overloading.html"])
	| CompilerGenerated -> ":compilerGenerated",("Marks a field as generated by the compiler. Should not be used by the end user.",[Platforms [Java;Cs]])
	| Const -> ":const",("Allows a type parameter to accept expression values.",[UsedOn [TTypeParameter]])
	| CoreApi -> ":coreApi",("Identifies this class as a core API class (forces API check).",[UsedOn [TClass;TEnum;TTypedef;TAbstract]])
	| CoreType -> ":coreType",("Identifies an abstract as core type so that it requires no implementation.",[UsedOn [TAbstract]; Link "https://haxe.org/manual/types-abstract-core-type.html"])
	| CppFileCode -> ":cppFileCode",("Code to be injected into generated cpp file.",[Platforms [Cpp]])
	| CppInclude -> ":cppInclude",("File to be included in generated cpp file.",[Platforms [Cpp]])
	| CppNamespaceCode -> ":cppNamespaceCode",("",[Platforms [Cpp]])
	| CsNative -> ":csNative",("Automatically added by `--net-lib` on classes generated from .NET DLL files.",[Platforms [Cs]; UsedOn [TClass;TEnum]; UsedInternally])
	| Dce -> ":dce",("Forces dead code elimination even when `--dce full` is not specified.",[UsedOn [TClass;TEnum]; Link "https://haxe.org/manual/cr-dce.html"])
	| Debug -> ":debug",("Forces debug information to be generated into the SWF even without `--debug`.",[Platforms [Flash]; UsedOn [TClass;TClassField]])
	| Decl -> ":decl",("",[Platforms [Cpp]])
	| DefParam -> ":defParam",("Default function argument value loaded from the SWF and used for documentation in Genxml.",[Platforms [Flash]; UsedInternally])
	| Delegate -> ":delegate",("Automatically added by `--net-lib` on delegates.",[Platforms [Cs]; UsedOn [TAbstract]])
	| Depend -> ":depend",("",[Platforms [Cpp]])
	| Deprecated -> ":deprecated",("Mark a type or field as deprecated.",[])
	| DirectlyUsed -> ":directlyUsed",("Marks types that are directly referenced by non-extern code.",[UsedInternally])
	| DisplayOverride -> ":?display.override",("Internally used to mark override fields for completion",[UsedInternally])
	| DynamicObject -> ":dynamicObject",("Used internally to identify the Dynamic Object implementation.",[Platforms [Java;Cs]; UsedOn [TClass]; UsedInternally])
	| Eager -> ":eager",("Forces typedefs to be followed early.",[UsedOn [TTypedef]])
	| Enum -> ":enum",("Defines finite value sets to abstract definitions.",[UsedOn [TAbstract]; Link "https://haxe.org/manual/types-abstract-enum.html"])
	| EnumConstructorParam -> ":enumConstructorParam",("Used internally to annotate GADT type parameters.",[UsedOn [TClass]; UsedInternally])
	| Event -> ":event",("Automatically added by `--net-lib` on events. Has no effect on types compiled by Haxe.",[Platforms [Cs]; UsedOn [TClassField]])
	| Exhaustive -> ":exhaustive",("Used internally to mark that a switch is exhaustive.",[UsedInternally; Link "https://haxe.org/manual/lf-pattern-matching-exhaustiveness.html"])
	| Expose -> ":expose",("Includes the class or field in Haxe exports (default name is the classpath).",[Platforms [Js;Lua]; HasParam "name"; Link "https://haxe.org/manual/target-javascript-expose.html"])
	| Extern -> ":extern",("Marks the field as extern so it is not generated.",[UsedOn [TClassField]])
	| File -> ":file",("Includes a given binary file into the target SWF and associates it with the class (must extend `flash.utils.ByteArray`).",[Platforms [Flash]; HasParam "File path"; UsedOn [TClass]; Link "https://haxe.org/manual/target-flash-resources.html"])
	| FileXml -> ":fileXml",("Include a given XML attribute snippet in the `Build.xml` entry for the file.",[Platforms [Cpp]; UsedOn [TClass]])
	| Final -> ":final",("Prevents a class or interface from being extended or a method from being overridden. Deprecated by the keyword `final`.",[UsedOn [TClass;TClassField]; Link "https://haxe.org/manual/class-field-final.html"])
	| Fixed -> ":fixed",("Declares an anonymous object to have fixed fields.",[])
	| FlashProperty -> ":flash.property",("",[Platforms [Flash]; UsedOn [TClassField]])
	| FlatEnum -> ":flatEnum",("Internally used to mark an enum as being flat, i.e. having no function constructors.",[UsedOn [TEnum]; UsedInternally])
	| Font -> ":font",("Embeds the given TrueType font into the class (must extend `flash.text.Font`).",[HasParam "TTF path"; HasParam "Range String"; UsedOn [TClass]; Link "https://haxe.org/manual/target-flash-resources.html"])
	| ForLoopVariable -> ":forLoopVariable",("Internally used to mark for-loop variables.",[UsedInternally])
	| Forward -> ":forward",("Forwards field access to underlying type.",[HasParam "List of field names"; UsedOn [TAbstract]; Link "https://haxe.org/manual/types-abstract-forward.html"])
	| ForwardStatics -> ":forwardStatics",("Forwards static field access to underlying type.",[HasParam "List of field names"; UsedOn [TAbstract]; Link "https://haxe.org/manual/types-abstract-forward.html"])
	| From -> ":from",("Specifies that the field of the abstract is a cast operation from the type identified in the function.",[UsedOn [TAbstractField]; Link "https://haxe.org/manual/types-abstract-implicit-casts.html"])
	| FunctionCode -> ":functionCode",("Used to inject platform-native code into a function.",[Platforms [Cpp;Java;Cs]])
	| FunctionTailCode -> ":functionTailCode",("",[Platforms [Cpp]])
	| Generic -> ":generic",("Marks a class or class field as generic so each type parameter combination generates its own type/field.",[Link "https://haxe.org/manual/type-system-generic.html"])
	| GenericBuild -> ":genericBuild",("Builds instances of a type using the specified macro.",[UsedOn [TClass]])
	| GenericInstance -> ":genericInstance",("Internally used to mark instances of `@:generic` methods.",[UsedOn [TClassField]; UsedInternally])
	| Getter -> ":getter",("Generates a native getter function on the given field.",[Platforms [Flash]; HasParam "Class field name"; UsedOn [TClassField]])
	| Hack -> ":hack",("Allows extending classes marked as `@:final`. Not guaranteed to work on all targets.",[UsedOn [TClass]])
	| HasUntyped -> ":has_untyped",("Used by the typer to mark fields that have untyped expressions.",[UsedInternally])
	| HaxeGeneric -> ":haxeGeneric",("Used internally to annotate non-native generic classes.",[Platforms [Cs]; UsedOn [TClass;TEnum]; UsedInternally])
	| HeaderClassCode -> ":headerClassCode",("Code to be injected into the generated class, in the header.",[Platforms [Cpp]])
	| HeaderCode -> ":headerCode",("Code to be injected into the generated header file.",[Platforms [Cpp]])
	| HeaderInclude -> ":headerInclude",("File to be included in generated header file.",[Platforms [Cpp]])
	| HeaderNamespaceCode -> ":headerNamespaceCode",("",[Platforms [Cpp]])
	| HlNative -> ":hlNative",("Specifies `hdll` name and function prefix for native functions.",[Platforms [Hl]; UsedOn [TClass;TClassField]])
	| HxGen -> ":hxGen",("Annotates that an extern class was generated by Haxe.",[Platforms [Java;Cs]; UsedOn [TClass;TEnum]])
	| IfFeature -> ":ifFeature",("Causes a field to be kept by DCE if the given feature is part of the compilation.",[HasParam "Feature name"; UsedOn [TClassField]; Link "https://haxe.org/manual/cr-dce.html"])
	| Impl -> ":impl",("Used internally to mark abstract implementation fields.",[UsedOn [TAbstractField]; UsedInternally])
	| PythonImport -> ":pythonImport",("Generates python import statement for extern classes.",[Platforms [Python]; UsedOn [TClass]])
	| ImplicitCast -> ":implicitCast",("Generated automatically on the AST when an implicit abstract cast happens.",[UsedOn [TExpr]; UsedInternally])
	| ImplicitReturn -> ":implicitReturn",("Generated automatically on the AST when an implicit return is inserted for arrow function.",[UsedOn [TExpr]; UsedInternally])
	| Include -> ":include",("",[Platforms [Cpp]])
	| InitPackage -> ":initPackage",("Some weird thing for Genjs we want to remove someday.",[Platforms [Js]; UsedInternally])
	| Inline -> ":inline",("Inserted by the parser in case of `inline expr` and `inline function`.",[UsedOn [TExpr]])
	| InlineConstructorArgument _ -> ":inlineConstructorArgument",("Internally used to mark expressions that were passed as arguments of an inlined constructor.",[UsedInternally])
	| Internal -> ":internal",("Generates the annotated field/class with 'internal' access.",[Platforms [Java;Cs]; UsedOn [TClass;TEnum;TClassField]])
	| IsVar -> ":isVar",("Forces a physical field to be generated for properties that otherwise would not require one.",[UsedOn [TClassField]; Link "https://haxe.org/manual/class-field-property-rules.html"])
	| JavaCanonical -> ":javaCanonical",("Used by the Java target to annotate the canonical path of the type.",[Platforms [Java]; HasParam "Output type package"; HasParam "Output type name"; UsedOn [TClass;TEnum]])
	| JavaNative -> ":javaNative",("Automatically added by `--java-lib` on classes generated from JAR/class files.",[Platforms [Java]; UsedOn [TClass;TEnum]; UsedInternally])
	| JsRequire -> ":jsRequire",("Generate JavaScript module require expression for given extern.",[Platforms [Js]; UsedOn [TClass]; Link "https://haxe.org/manual/target-javascript-require.html"])
	| LuaRequire -> ":luaRequire",("Generate Lua module require expression for given extern.",[Platforms [Lua]; UsedOn [TClass]])
	| LuaDotMethod -> ":luaDotMethod",("Indicates that the given extern type instance should have dot-style invocation for methods instead of colon.",[Platforms [Lua]])
	| Keep -> ":keep",("Causes a field or type to be kept by DCE.",[Link "https://haxe.org/manual/cr-dce.html"])
	| KeepInit -> ":keepInit",("Causes a class to be kept by DCE even if all its field are removed.",[UsedOn [TClass]; Link "https://haxe.org/manual/cr-dce.html"])
	| KeepSub -> ":keepSub",("Extends `@:keep` metadata to all implementing and extending classes.",[UsedOn [TClass]; Link "https://haxe.org/manual/cr-dce.html"])
	| LibType -> ":libType",("Used by `--net-lib` and `--java-lib` to mark a class that should not be checked (overrides, interfaces, etc) by the type loader.",[Platforms [Java;Cs]; UsedOn [TClass]; UsedInternally])
	| LoopLabel -> ":loopLabel",("Mark loop and break expressions with a label to support breaking from within switch.",[UsedInternally])
	| Markup -> ":markup",("Used as a result of inline XML parsing.",[Link "https://haxe.org/manual/lf-markup.html"])
	| Meta -> ":meta",("Internally used to mark a class field as being the metadata field.",[UsedInternally])
	| Macro -> ":macro",("(deprecated)",[])
	| MaybeUsed -> ":maybeUsed",("Internally used by DCE to mark fields that might be kept.",[UsedInternally])
	| MergeBlock -> ":mergeBlock",("Merge the annotated block into the current scope.",[UsedOn [TExpr]])
	| MultiReturn -> ":multiReturn",("Annotates an extern class as the result of multi-return function.",[Platforms [Lua]; UsedOn [TClass]; Link "https://haxe.org/manual/target-lua-multireturns.html"])
	| MultiType -> ":multiType",("Specifies that an abstract chooses its this-type from its `@:to` functions.",[HasParam "Relevant type parameters"; UsedOn [TAbstract]])
	| Native -> ":native",("Rewrites the path of a class or enum during generation.",[HasParam "Output type path"; UsedOn [TClass;TEnum]; Link "https://haxe.org/manual/lf-externs.html"])
	| NativeChildren -> ":nativeChildren",("Annotates that all children from a type should be treated as if it were an extern definition - platform native.",[Platforms [Java;Cs]; UsedOn [TClass]])
	| NativeGen -> ":nativeGen",("Annotates that a type should be treated as if it were an extern definition - platform native.",[Platforms [Java;Cs;Python]; UsedOn [TClass;TEnum]])
	| NativeGeneric -> ":nativeGeneric",("Used internally to annotate native generic classes.",[Platforms [Cs]; UsedOn [TClass;TEnum]; UsedInternally])
	| NativeProperty -> ":nativeProperty",("Use native properties which will execute even with dynamic usage.",[Platforms [Cpp]])
	| NativeStaticExtension -> ":nativeStaticExtension",("Converts static function syntax into member call.",[Platforms [Cpp]])
	| NoCompletion -> ":noCompletion",("Prevents the compiler from suggesting completion on this field or type.",[UsedOn [TClassField]; Link "https://haxe.org/manual/cr-completion.html"])
	| NoClosure -> ":noClosure",("Prevents a method or all methods in a class from being used as a value.",[UsedOn [TClass;TClassField]])
	| NoDebug -> ":noDebug",("Does not generate debug information even if `--debug` is set.",[Platforms [Flash;Cpp]; UsedOn [TClass;TClassField]])
	| NoDoc -> ":noDoc",("Prevents a type from being included in documentation generation.",[])
	| NoExpr -> ":noExpr",("Internally used to mark abstract fields which have no expression by design.",[UsedInternally])
	| NoImportGlobal -> ":noImportGlobal",("Prevents a static field from being imported with `import Class.*`.",[UsedOn [TAnyField]])
	| NonVirtual -> ":nonVirtual",("Declares function to be non-virtual in cpp.",[Platforms [Cpp]])
	| NoPackageRestrict -> ":noPackageRestrict",("Allows a module to be accessed across all targets if found on its first type.",[UsedInternally])
	| NoPrivateAccess -> ":noPrivateAccess",("Disallow private access to anything for the annotated expression.",[UsedOn [TExpr]])
	| NoStack -> ":noStack",("",[Platforms [Cpp]])
	| NotNull -> ":notNull",("Declares an abstract type as not accepting null values.",[UsedOn [TAbstract]; Link "https://haxe.org/manual/types-nullability.html"])
	| NoUsing -> ":noUsing",("Prevents a field from being used with static extension.",[UsedOn [TClassField]; Link "https://haxe.org/manual/lf-static-extension.html"])
	| Ns -> ":ns",("Internally used by the SWF generator to handle namespaces.",[Platforms [Flash]; UsedInternally])
	| NullSafety -> ":nullSafety",("Enables null safety for classes or fields. Disables null safety for classes, fields or expressions if provided with `Off` as an argument.",[HasParam "Off | Loose | Strict"; UsedOn [TClass;TClassField;TExpr]; Link "https://haxe.org/manual/cr-null-safety.html"])
	| Objc -> ":objc",("Declares a class or interface that is used to interoperate with Objective-C code.",[Platforms [Cpp]; UsedOn [TClass]])
	| ObjcProtocol -> ":objcProtocol",("Associates an interface with, or describes a function in, a native Objective-C protocol.",[Platforms [Cpp]])
	| Op -> ":op",("Declares an abstract field as being an operator overload.",[HasParam "The operation"; UsedOn [TAbstractField]; Link "https://haxe.org/manual/types-abstract-operator-overloading.html"])
	| Optional -> ":optional",("Marks the field of a structure as optional.",[UsedOn [TClassField]; Link "https://haxe.org/manual/types-nullability-optional-arguments.html"])
	| Overload -> ":overload",("Allows the field to be called with different argument types.",[HasParam "Function specification (no expression)"; UsedOn [TClassField]; Link "https://haxe.org/manual/target-javascript-external-libraries.html"])
	| Persistent -> ":persistent",("Keeps the value of static variables in macro context across compilations.",[Platforms [Eval]; UsedOn [TAnyField]])
	| PhpGlobal -> ":phpGlobal",("Indicates that static fields of an extern class actually are located in the global PHP namespace.",[Platforms [Php]; UsedOn [TClass]])
	| PhpClassConst -> ":phpClassConst",("Indicates that a static var of an extern class is a PHP class constant.",[Platforms [Php]; UsedOn [TClassField]])
	| PhpMagic -> ":phpMagic",("Treat annotated field as special PHP magic field - this meta makes compiler avoid renaming such fields on generating PHP code.",[Platforms [Php]; UsedOn [TClassField]])
	| PhpNoConstructor -> ":phpNoConstructor",("Special meta for extern classes which do not have native constructor in PHP, but need a constructor in Haxe extern.",[Platforms [Php]; UsedOn [TClass]])
	| Pos -> ":pos",("Sets the position of a reified expression.",[HasParam "Position"; UsedOn [TExpr]; Link "https://haxe.org/manual/macro-reification.html"])
	| Public -> ":public",("Marks a class field as being public.",[UsedOn [TClassField]; UsedInternally])
	| PublicFields -> ":publicFields",("Forces all class fields of inheriting classes to be public.",[UsedOn [TClass]])
	| Private -> ":private",("Marks a class field as being private.",[Platforms [Cs]; UsedOn [TClassField]])
	| PrivateAccess -> ":privateAccess",("Allow private access to anything for the annotated expression.",[UsedOn [TExpr]])
	| Protected -> ":protected",("Marks a class field as being protected.",[Platforms [Cs;Java;Flash]; UsedOn [TClassField]])
	| Property -> ":property",("Marks a property field to be compiled as a native C# property.",[Platforms [Cs]; UsedOn [TClassField]])
	| Pure -> ":pure",("Marks a class field, class or expression as pure (side-effect free).",[UsedOn [TClass;TClassField;TExpr]])
	| ReadOnly -> ":readOnly",("Generates a field with the `readonly` native keyword.",[Platforms [Cs]; UsedOn [TClassField]])
	| RealPath -> ":realPath",("Internally used on `@:native` types to retain original path information.",[UsedInternally])
	| Remove -> ":remove",("Causes an interface to be removed from all implementing classes before generation.",[UsedOn [TClass]])
	| Require -> ":require",("Allows access to a field only if the specified compiler flag is set.",[HasParam "Compiler flag to check"; UsedOn [TClassField]; Link "https://haxe.org/manual/lf-condition-compilation.html"])
	| RequiresAssign -> ":requiresAssign",("Used internally to mark certain abstract operator overloads.",[UsedInternally])
	| Resolve -> ":resolve",("Abstract fields marked with this metadata can be used to resolve unknown fields.",[UsedOn [TClassField]])
	| Rtti -> ":rtti",("Adds runtime type information.",[UsedOn [TClass]; Link "https://haxe.org/manual/cr-rtti.html"])
	| Runtime -> ":runtime",("",[UsedInternally])
	| RuntimeValue -> ":runtimeValue",("Marks an abstract as being a runtime value.",[UsedOn [TAbstract]])
	| Scalar -> ":scalar",("Used by hxcpp to mark a custom coreType abstract.",[Platforms [Cpp]; UsedOn [TAbstract]])
	| SelfCall -> ":selfCall",("Translates method calls into calling object directly.",[Platforms [Js;Lua]; UsedOn [TClassField]; Link "https://haxe.org/manual/target-javascript-external-libraries.html"])
	| Semantics -> ":semantics",("The native semantics of the type.",[HasParam "value | reference | variable"; UsedOn [TClass;TTypedef;TAbstract]])
	| Setter -> ":setter",("Generates a native setter function on the given field.",[Platforms [Flash]; HasParam "Class field name"; UsedOn [TClassField]])
	| SkipCtor -> ":skipCtor",("Used internally to generate a constructor as if it were a native type (no `__hx_ctor`).",[Platforms [Java;Cs]; UsedInternally])
	| SkipReflection -> ":skipReflection",("Used internally to annotate a field that shouldn't have its reflection data generated.",[Platforms [Java;Cs]; UsedOn [TClassField]; UsedInternally])
	| Sound -> ":sound",("Includes a given .wav or .mp3 file into the target SWF and associates it with the class (must extend `flash.media.Sound`).",[Platforms [Flash]; HasParam "File path"; UsedOn [TClass]; Link "https://haxe.org/manual/target-flash-resources.html"])
	| SourceFile -> ":sourceFile",("Source code filename for external class.",[Platforms [Cpp]])
	| StackOnly -> ":stackOnly",("Instances of this type can only appear on the stack.",[Platforms [Cpp]])
	| StaticExtension -> "haxe.internal.static_extension",("Used internally to mark static extension fields.",[UsedInternally])
	| StoredTypedExpr -> ":storedTypedExpr",("Used internally to reference a typed expression returned from a macro.",[UsedInternally])
	| Strict -> ":strict",("Used to declare a native C# attribute or a native Java metadata; is type checked.",[Platforms [Java;Cs]])
	| Struct -> ":struct",("Marks a class definition as a struct.",[Platforms [Cs;Hl]; UsedOn [TClass]])
	| StructAccess -> ":structAccess",("Marks an extern class as using struct access (`.`) not pointer (`->`).",[Platforms [Cpp]; UsedOn [TClass]])
	| StructInit -> ":structInit",("Allows one to initialize the class with a structure that matches constructor parameters.",[UsedOn [TClass]])
	| SuppressWarnings -> ":suppressWarnings",("Adds a `SuppressWarnings` annotation for the generated Java class.",[Platforms [Java]; UsedOn [TClass]])
	| TemplatedCall -> ":templatedCall",("Indicates that the first parameter of static call should be treated as a template argument.",[Platforms [Cpp]; UsedOn [TClassField]])
	| Throws -> ":throws",("Adds a `throws` declaration to the generated function.",[Platforms [Java]; HasParam "Type as String"; UsedOn [TClassField]])
	| This -> ":this",("Internally used to pass a `this` expression to macros.",[UsedOn [TExpr]; UsedInternally])
	| To -> ":to",("Specifies that the field of the abstract is a cast operation to the type identified in the function.",[UsedOn [TAbstractField]; Link "https://haxe.org/manual/types-abstract-implicit-casts.html"])
	| ToString -> ":toString",("Internally used.",[UsedInternally])
	| Transient -> ":transient",("Adds the `transient` flag to the class field.",[Platforms [Java]; UsedOn [TClassField]])
	| ValueUsed -> ":valueUsed",("Internally used by DCE to mark an abstract value as used.",[UsedInternally])
	| Volatile -> ":volatile",("",[Platforms [Java;Cs]])
	| UnifyMinDynamic -> ":unifyMinDynamic",("Allows a collection of types to unify to `Dynamic`.",[UsedOn [TClassField]])
	| Unreflective -> ":unreflective",("",[Platforms [Cpp]])
	| Unsafe -> ":unsafe",("Declares a class, or a method with the C#'s `unsafe` flag.",[Platforms [Cs]; UsedOn [TClass;TClassField]])
	| Used -> ":used",("Internally used by DCE to mark a class or field as used.",[UsedInternally])
	| Using -> ":using",("Automatically uses the argument types as static extensions for the annotated type.",[UsedOn [TClass;TEnum;TAbstract]; Link "https://haxe.org/manual/lf-static-extension-metadata.html"])
	| Value -> ":value",("Used to store default values for fields and function arguments.",[UsedOn [TClassField]])
	| Void -> ":void",("Use Cpp native `void` return type.",[Platforms [Cpp]])
	| Last -> assert false
	| Dollar s -> "$" ^ s,("",[])
	| Custom s -> s,("",[])
