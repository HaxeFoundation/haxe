open Globals

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
	| DynamicObject
	| Eager
	| Enum
	| EnumConstructorParam
	| Event
	| Exhaustive
	| Expose
	| Extern
	| FakeEnum
	| File
	| FileXml
	| Final
	| Fixed
	| FlatEnum
	| Font
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
	| HxGen
	| IfFeature
	| Impl
	| PythonImport
	| ImplicitCast
	| Include
	| InitPackage
	| InlineConstructorVariable
	| Internal
	| IsVar
	| JavaCanonical
	| JavaNative
	| JsRequire
	| Keep
	| KeepInit
	| KeepSub
	| LibType
	| LuaRequire
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
	| Objc
	| ObjcProtocol
	| Op
	| Optional
	| Overload
	| PhpConstants
	| PhpGlobal
	| PrivateAccess
	| Property
	| Protected
	| Public
	| PublicFields
	| Pure
	| QuotedField
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
	| Setter
	| SkipCtor
	| SkipReflection
	| Sound
	| SourceFile
	| StackOnly
	| StoredTypedExpr
	| Strict
	| Struct
	| StructAccess
	| StructInit
	| SuppressWarnings
	| This
	| Throws
	| To
	| ToString
	| Transient
	| TemplatedCall
	| ValueUsed
	| Volatile
	| Unbound
	| UnifyMinDynamic
	| Unreflective
	| Unsafe
	| Usage
	| Used
	| UserVariable
	| Value
	| Void
	| Last
	(* do not put any custom metadata after Last *)
	| Dollar of string
	| Custom of string

let has m ml = List.exists (fun (m2,_,_) -> m = m2) ml
let get m ml = List.find (fun (m2,_,_) -> m = m2) ml

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
	| UsedInternally

let get_info = function
	| Abi -> ":abi",("Function ABI/calling convention",[Platforms [Cpp]])
	| Abstract -> ":abstract",("Sets the underlying class implementation as 'abstract'",[Platforms [Java;Cs]])
	| Access -> ":access",("Forces private access to package, type or field",[HasParam "Target path";UsedOnEither [TClass;TClassField]])
	| Accessor -> ":accessor",("Used internally by DCE to mark property accessors",[UsedOn TClassField;UsedInternally])
	| Allow -> ":allow",("Allows private access from package, type or field",[HasParam "Target path";UsedOnEither [TClass;TClassField]])
	| Analyzer -> ":analyzer",("Used to configure the static analyzer",[])
	| Annotation -> ":annotation",("Annotation (@interface) definitions on -java-lib imports will be annotated with this metadata. Has no effect on types compiled by Haxe",[Platform Java; UsedOn TClass])
	| ArrayAccess -> ":arrayAccess",("Allows [] access on an abstract",[UsedOnEither [TAbstract;TAbstractField]])
	| Ast -> ":ast",("Internally used to pass the AST source into the typed AST",[UsedInternally])
	| AstSource -> ":astSource",("Filled by the compiler with the parsed expression of the field",[UsedOn TClassField])
	| AutoBuild -> ":autoBuild",("Extends @:build metadata to all extending and implementing classes",[HasParam "Build macro call";UsedOn TClass])
	| Bind -> ":bind",("Override Swf class declaration",[Platform Flash;UsedOn TClass])
	| Bitmap -> ":bitmap",("Embeds given bitmap data into the class (must extend flash.display.BitmapData)",[HasParam "Bitmap file path";UsedOn TClass;Platform Flash])
	| BridgeProperties -> ":bridgeProperties",("Creates native property bridges for all Haxe properties in this class",[UsedOn TClass;Platform Cs])
	| Build -> ":build",("Builds a class or enum from a macro",[HasParam "Build macro call";UsedOnEither [TClass;TEnum]])
	| BuildXml -> ":buildXml",("Specify xml data to be injected into Build.xml",[Platform Cpp])
	| Callable -> ":callable",("Abstract forwards call to its underlying type",[UsedOn TAbstract])
	| Class -> ":class",("Used internally to annotate an enum that will be generated as a class",[Platforms [Java;Cs]; UsedOn TEnum; UsedInternally])
	| ClassCode -> ":classCode",("Used to inject platform-native code into a class",[Platforms [Java;Cs]; UsedOn TClass])
	| Commutative -> ":commutative",("Declares an abstract operator as commutative",[UsedOn TAbstractField])
	| CompilerGenerated -> ":compilerGenerated",("Marks a field as generated by the compiler. Shouldn't be used by the end user",[Platforms [Java;Cs]])
	| Const -> ":const",("Allows a type parameter to accept expression values",[UsedOn TTypeParameter])
	| CoreApi -> ":coreApi",("Identifies this class as a core api class (forces Api check)",[UsedOnEither [TClass;TEnum;TTypedef;TAbstract]])
	| CoreType -> ":coreType",("Identifies an abstract as core type so that it requires no implementation",[UsedOn TAbstract])
	| CppFileCode -> ":cppFileCode",("Code to be injected into generated cpp file",[Platform Cpp])
	| CppInclude -> ":cppInclude",("File to be included in generated cpp file",[Platform Cpp])
	| CppNamespaceCode -> ":cppNamespaceCode",("",[Platform Cpp])
	| CsNative -> ":csNative",("Automatically added by -net-lib on classes generated from .NET DLL files",[Platform Cs; UsedOnEither[TClass;TEnum]; UsedInternally])
	| Dce -> ":dce",("Forces dead code elimination even when -dce full is not specified",[UsedOnEither [TClass;TEnum]])
	| Debug -> ":debug",("Forces debug information to be generated into the Swf even without -debug",[UsedOnEither [TClass;TClassField]; Platform Flash])
	| Decl -> ":decl",("",[Platform Cpp])
	| DefParam -> ":defParam",("Default function argument value loaded from the SWF and used for documentation in Genxml",[Platform Flash;UsedInternally])
	| Delegate -> ":delegate",("Automatically added by -net-lib on delegates",[Platform Cs; UsedOn TAbstract])
	| Depend -> ":depend",("",[Platform Cpp])
	| Deprecated -> ":deprecated",("Mark a type or field as deprecated",[])
	| DirectlyUsed -> ":directlyUsed",("Marks types that are directly referenced by non-extern code",[UsedInternally])
	| DynamicObject -> ":dynamicObject",("Used internally to identify the Dynamic Object implementation",[Platforms [Java;Cs]; UsedOn TClass; UsedInternally])
	| Eager -> ":eager",("Forces typedefs to be followed early",[UsedOn TTypedef])
	| Enum -> ":enum",("Defines finite value sets to abstract definitions",[UsedOn TAbstract])
	| EnumConstructorParam -> ":enumConstructorParam",("Used internally to annotate GADT type parameters",[UsedOn TClass; UsedInternally])
	| Event -> ":event",("Automatically added by -net-lib on events. Has no effect on types compiled by Haxe",[Platform Cs; UsedOn TClassField])
	| Exhaustive -> ":exhaustive",("",[UsedInternally])
	| Expose -> ":expose",("Makes the class available on the window object",[HasParam "?Name=Class path";UsedOn TClass;Platform Js])
	| Extern -> ":extern",("Marks the field as extern so it is not generated",[UsedOn TClassField])
	| FakeEnum -> ":fakeEnum",("Treat enum as collection of values of the specified type",[HasParam "Type name";UsedOn TEnum])
	| File -> ":file",("Includes a given binary file into the target Swf and associates it with the class (must extend flash.utils.ByteArray)",[HasParam "File path";UsedOn TClass;Platform Flash])
	| FileXml -> ":fileXml",("Include xml attribute snippet in Build.xml entry for file",[UsedOn TClass;Platform Cpp])
	| Final -> ":final",("Prevents a class from being extended",[UsedOn TClass])
	| Fixed -> ":fixed",("Delcares an anonymous object to have fixed fields",[ (*UsedOn TObjectDecl(_)*)])
	| FlatEnum -> ":flatEnum",("Internally used to mark an enum as being flat, i.e. having no function constructors",[UsedOn TEnum; UsedInternally])
	| Font -> ":font",("Embeds the given TrueType font into the class (must extend flash.text.Font)",[HasParam "TTF path";HasParam "Range String";UsedOn TClass])
	| Forward -> ":forward",("Forwards field access to underlying type",[HasParam "List of field names";UsedOn TAbstract])
	| ForwardStatics -> ":forwardStatics",("Forwards static field access to underlying type",[HasParam "List of field names";UsedOn TAbstract])
	| From -> ":from",("Specifies that the field of the abstract is a cast operation from the type identified in the function",[UsedOn TAbstractField])
	| FunctionCode -> ":functionCode",("Used to inject platform-native code into a function",[Platforms [Cpp;Java;Cs]])
	| FunctionTailCode -> ":functionTailCode",("",[Platform Cpp])
	| Generic -> ":generic",("Marks a class or class field as generic so each type parameter combination generates its own type/field",[UsedOnEither [TClass;TClassField]])
	| GenericBuild -> ":genericBuild",("Builds instances of a type using the specified macro",[UsedOn TClass])
	| GenericInstance -> ":genericInstance",("Internally used to mark instances of @:generic methods",[UsedOn TClassField;UsedInternally])
	| Getter -> ":getter",("Generates a native getter function on the given field",[HasParam "Class field name";UsedOn TClassField;Platform Flash])
	| Hack -> ":hack",("Allows extending classes marked as @:final",[UsedOn TClass])
	| HasUntyped -> (":has_untyped",("Used by the typer to mark fields that have untyped expressions",[UsedInternally]))
	| HaxeGeneric -> ":haxeGeneric",("Used internally to annotate non-native generic classes",[Platform Cs; UsedOnEither[TClass;TEnum]; UsedInternally])
	| HeaderClassCode -> ":headerClassCode",("Code to be injected into the generated class, in the header",[Platform Cpp])
	| HeaderCode -> ":headerCode",("Code to be injected into the generated header file",[Platform Cpp])
	| HeaderInclude -> ":headerInclude",("File to be included in generated header file",[Platform Cpp])
	| HeaderNamespaceCode -> ":headerNamespaceCode",("",[Platform Cpp])
	| HxGen -> ":hxGen",("Annotates that an extern class was generated by Haxe",[Platforms [Java;Cs]; UsedOnEither [TClass;TEnum]])
	| IfFeature -> ":ifFeature",("Causes a field to be kept by DCE if the given feature is part of the compilation",[HasParam "Feature name";UsedOn TClassField])
	| Impl -> ":impl",("Used internally to mark abstract implementation fields",[UsedOn TAbstractField; UsedInternally])
	| PythonImport -> ":pythonImport",("Generates python import statement for extern classes",[Platforms [Python]; UsedOn TClass])
	| ImplicitCast -> ":implicitCast",("Generated automatically on the AST when an implicit abstract cast happens",[UsedInternally; UsedOn TExpr])
	| Include -> ":include",("",[Platform Cpp])
	| InitPackage -> ":initPackage",("Some weird thing for Genjs we want to remove someday",[UsedInternally; Platform Js])
	| InlineConstructorVariable -> ":inlineConstructorVariable",("Internally used to mark variables that come from inlined constructors",[UsedInternally])
	| Internal -> ":internal",("Generates the annotated field/class with 'internal' access",[Platforms [Java;Cs]; UsedOnEither[TClass;TEnum;TClassField]])
	| IsVar -> ":isVar",("Forces a physical field to be generated for properties that otherwise would not require one",[UsedOn TClassField])
	| JavaCanonical -> ":javaCanonical",("Used by the Java target to annotate the canonical path of the type",[HasParam "Output type package";HasParam "Output type name";UsedOnEither [TClass;TEnum]; Platform Java])
	| JavaNative -> ":javaNative",("Automatically added by -java-lib on classes generated from JAR/class files",[Platform Java; UsedOnEither[TClass;TEnum]; UsedInternally])
	| JsRequire -> ":jsRequire",("Generate javascript module require expression for given extern",[Platform Js; UsedOn TClass])
	| LuaRequire -> ":luaRequire",("Generate lua module require expression for given extern",[Platform Lua; UsedOn TClass])
	| Keep -> ":keep",("Causes a field or type to be kept by DCE",[])
	| KeepInit -> ":keepInit",("Causes a class to be kept by DCE even if all its field are removed",[UsedOn TClass])
	| KeepSub -> ":keepSub",("Extends @:keep metadata to all implementing and extending classes",[UsedOn TClass])
	| LibType -> ":libType",("Used by -net-lib and -java-lib to mark a class that shouldn't be checked (overrides, interfaces, etc) by the type loader",[UsedInternally; UsedOn TClass; Platforms [Java;Cs]])
	| Meta -> ":meta",("Internally used to mark a class field as being the metadata field",[])
	| Macro -> ":macro",("(deprecated)",[])
	| MaybeUsed -> ":maybeUsed",("Internally used by DCE to mark fields that might be kept",[UsedInternally])
	| MergeBlock -> ":mergeBlock",("Merge the annotated block into the current scope",[UsedOn TExpr])
	| MultiReturn -> ":multiReturn",("Annotates an extern class as the result of multi-return function",[UsedOn TClass; Platform Lua])
	| MultiType -> ":multiType",("Specifies that an abstract chooses its this-type from its @:to functions",[UsedOn TAbstract; HasParam "Relevant type parameters"])
	| Native -> ":native",("Rewrites the path of a class or enum during generation",[HasParam "Output type path";UsedOnEither [TClass;TEnum]])
	| NativeChildren -> ":nativeChildren",("Annotates that all children from a type should be treated as if it were an extern definition - platform native",[Platforms [Java;Cs]; UsedOn TClass])
	| NativeGen -> ":nativeGen",("Annotates that a type should be treated as if it were an extern definition - platform native",[Platforms [Java;Cs;Python]; UsedOnEither[TClass;TEnum]])
	| NativeGeneric -> ":nativeGeneric",("Used internally to annotate native generic classes",[Platform Cs; UsedOnEither[TClass;TEnum]; UsedInternally])
	| NativeProperty -> ":nativeProperty",("Use native properties which will execute even with dynamic usage",[Platform Cpp])
	| NativeStaticExtension -> ":nativeStaticExtension",("Converts static function syntax into member call",[Platform Cpp])
	| NoCompletion -> ":noCompletion",("Prevents the compiler from suggesting completion on this field",[UsedOn TClassField])
	| NoDebug -> ":noDebug",("Does not generate debug information into the Swf even if -debug is set",[UsedOnEither [TClass;TClassField];Platform Flash])
	| NoDoc -> ":noDoc",("Prevents a type from being included in documentation generation",[])
	| NoExpr -> ":noExpr",("Internally used to mark abstract fields which have no expression by design",[UsedInternally])
	| NoImportGlobal -> ":noImportGlobal",("Prevents a static field from being imported with import Class.*",[UsedOn TAnyField])
	| NonVirtual -> ":nonVirtual",("Declares function to be non-virtual in cpp",[Platform Cpp])
	| NoPackageRestrict -> ":noPackageRestrict",("Allows a module to be accessed across all targets if found on its first type",[UsedInternally])
	| NoPrivateAccess -> ":noPrivateAccess",("Disallow private access to anything for the annotated expression",[UsedOn TExpr])
	| NoStack -> ":noStack",("",[Platform Cpp])
	| NotNull -> ":notNull",("Declares an abstract type as not accepting null values",[UsedOn TAbstract])
	| NoUsing -> ":noUsing",("Prevents a field from being used with 'using'",[UsedOn TClassField])
	| Ns -> ":ns",("Internally used by the Swf generator to handle namespaces",[Platform Flash])
	| Objc -> ":objc",("Declares a class or interface that is used to interoperate with Objective-C code",[Platform Cpp;UsedOn TClass])
	| ObjcProtocol -> ":objcProtocol",("Associates an interface with, or describes a function in, a native Objective-C protocol.",[Platform Cpp;UsedOnEither [TClass;TClassField] ])
	| Op -> ":op",("Declares an abstract field as being an operator overload",[HasParam "The operation";UsedOn TAbstractField])
	| Optional -> ":optional",("Marks the field of a structure as optional",[UsedOn TClassField])
	| Overload -> ":overload",("Allows the field to be called with different argument types",[HasParam "Function specification (no expression)";UsedOn TClassField])
	| PhpConstants -> ":phpConstants",("Marks the static fields of a class as PHP constants, without $",[Platform Php;UsedOn TClass])
	| PhpGlobal -> ":phpGlobal",("Puts the static fields of a class in the global PHP namespace",[Platform Php;UsedOn TClass])
	| Public -> ":public",("Marks a class field as being public",[UsedOn TClassField;UsedInternally])
	| PublicFields -> ":publicFields",("Forces all class fields of inheriting classes to be public",[UsedOn TClass])
	| QuotedField -> ":quotedField",("Used internally to mark structure fields which are quoted in syntax",[UsedInternally])
	| PrivateAccess -> ":privateAccess",("Allow private access to anything for the annotated expression",[UsedOn TExpr])
	| Protected -> ":protected",("Marks a class field as being protected",[UsedOn TClassField;Platforms [Cs;Java;Flash]])
	| Property -> ":property",("Marks a property field to be compiled as a native C# property",[UsedOn TClassField;Platform Cs])
	| Pure -> ":pure",("Marks a class field, class or expression as pure (side-effect free)",[UsedOnEither [TClass;TClassField;TExpr]])
	| ReadOnly -> ":readOnly",("Generates a field with the 'readonly' native keyword",[Platform Cs; UsedOn TClassField])
	| RealPath -> ":realPath",("Internally used on @:native types to retain original path information",[UsedInternally])
	| Remove -> ":remove",("Causes an interface to be removed from all implementing classes before generation",[UsedOn TClass])
	| Require -> ":require",("Allows access to a field only if the specified compiler flag is set",[HasParam "Compiler flag to check";UsedOn TClassField])
	| RequiresAssign -> ":requiresAssign",("Used internally to mark certain abstract operator overloads",[UsedInternally])
	| Resolve -> ":resolve",("Abstract fields marked with this metadata can be used to resolve unknown fields",[UsedOn TClassField])
	| Rtti -> ":rtti",("Adds runtime type informations",[UsedOn TClass])
	| Runtime -> ":runtime",("?",[])
	| RuntimeValue -> ":runtimeValue",("Marks an abstract as being a runtime value",[UsedOn TAbstract])
	| Scalar -> ":scalar",("Used by hxcpp to mark a custom coreType abstract",[UsedOn TAbstract; Platform Cpp])
	| SelfCall -> ":selfCall",("Translates method calls into calling object directly",[UsedOn TClassField; Platform Js])
	| Setter -> ":setter",("Generates a native setter function on the given field",[HasParam "Class field name";UsedOn TClassField;Platform Flash])
	| StackOnly -> ":stackOnly",("Instances of this type can only appear on the stack",[Platform Cpp])
	| StoredTypedExpr -> ":storedTypedExpr",("Used internally to reference a typed expression returned from a macro",[UsedInternally])
	| SkipCtor -> ":skipCtor",("Used internally to generate a constructor as if it were a native type (no __hx_ctor)",[Platforms [Java;Cs]; UsedInternally])
	| SkipReflection -> ":skipReflection",("Used internally to annotate a field that shouldn't have its reflection data generated",[Platforms [Java;Cs]; UsedOn TClassField; UsedInternally])
	| Sound -> ":sound",( "Includes a given .wav or .mp3 file into the target Swf and associates it with the class (must extend flash.media.Sound)",[HasParam "File path";UsedOn TClass;Platform Flash])
	| SourceFile -> ":sourceFile",("Source code filename for external class",[Platform Cpp])
	| Strict -> ":strict",("Used to declare a native C# attribute or a native Java metadata. Is type checked",[Platforms [Java;Cs]])
	| Struct -> ":struct",("Marks a class definition as a struct",[Platform Cs; UsedOn TClass])
	| StructAccess -> ":structAccess",("Marks an extern class as using struct access('.') not pointer('->')",[Platform Cpp; UsedOn TClass])
	| StructInit -> ":structInit",("Allows to initialize the class with a structure that matches constructor parameters",[UsedOn TClass])
	| SuppressWarnings -> ":suppressWarnings",("Adds a SuppressWarnings annotation for the generated Java class",[Platform Java; UsedOn TClass])
	| TemplatedCall -> ":templatedCall",("Indicates that the first parameter of static call should be treated as a template arguement",[Platform Cpp; UsedOn TClassField])
	| Throws -> ":throws",("Adds a 'throws' declaration to the generated function",[HasParam "Type as String"; Platform Java; UsedOn TClassField])
	| This -> ":this",("Internally used to pass a 'this' expression to macros",[UsedInternally; UsedOn TExpr])
	| To -> ":to",("Specifies that the field of the abstract is a cast operation to the type identified in the function",[UsedOn TAbstractField])
	| ToString -> ":toString",("Internally used",[UsedInternally])
	| Transient -> ":transient",("Adds the 'transient' flag to the class field",[Platform Java; UsedOn TClassField])
	| ValueUsed -> ":valueUsed",("Internally used by DCE to mark an abstract value as used",[UsedInternally])
	| Volatile -> ":volatile",("",[Platforms [Java;Cs]])
	| Unbound -> ":unbound", ("Compiler internal to denote unbounded global variable",[UsedInternally])
	| UnifyMinDynamic -> ":unifyMinDynamic",("Allows a collection of types to unify to Dynamic",[UsedOn TClassField])
	| Unreflective -> ":unreflective",("",[Platform Cpp])
	| Unsafe -> ":unsafe",("Declares a class, or a method with the C#'s 'unsafe' flag",[Platform Cs; UsedOnEither [TClass;TClassField]])
	| Usage -> ":usage",("Internal metadata used to mark a symbol for which usage request was invoked",[UsedInternally])
	| Used -> ":used",("Internally used by DCE to mark a class or field as used",[UsedInternally])
	| UserVariable -> ":userVariable",("Internally used to mark variables that come from user code",[UsedInternally])
	| Value -> ":value",("Used to store default values for fields and function arguments",[UsedOn TClassField])
	| Void -> ":void",("Use Cpp native 'void' return type",[Platform Cpp])
	| Last -> assert false
	(* do not put any custom metadata after Last *)
	| Dollar s -> "$" ^ s,("",[])
	| Custom s -> s,("",[])

let to_string m = fst (get_info m)

let hmeta =
	let h = Hashtbl.create 0 in
	let rec loop i =
		let m = Obj.magic i in
		if m <> Last then begin
			Hashtbl.add h (fst (get_info m)) m;
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

let get_documentation d =
	let t, (doc,flags) = get_info d in
	if not (List.mem UsedInternally flags) then begin
		let params = ref [] and used = ref [] and pfs = ref [] in
		List.iter (function
			| HasParam s -> params := s :: !params
			| Platform f -> pfs := f :: !pfs
			| Platforms fl -> pfs := fl @ !pfs
			| UsedOn u -> used := u :: !used
			| UsedOnEither ul -> used := ul @ !used
			| UsedInternally -> assert false
		) flags;
		let params = (match List.rev !params with
			| [] -> ""
			| l -> "(" ^ String.concat "," l ^ ")"
		) in
		let pfs = (match List.rev !pfs with
			| [] -> ""
			| [p] -> " (" ^ platform_name p ^ " only)"
			| pl -> " (for " ^ String.concat "," (List.map platform_name pl) ^ ")"
		) in
		let str = "@" ^ t in
		Some (str,params ^ doc ^ pfs)
	end else
		None

let get_documentation_list () =
	let m = ref 0 in
	let rec loop i =
		let d = Obj.magic i in
		if d <> Last then begin match get_documentation d with
			| None -> loop (i + 1)
			| Some (str,desc) ->
				if String.length str > !m then m := String.length str;
					(str,desc) :: loop (i + 1)
		end else
			[]
	in
	let all = List.sort (fun (s1,_) (s2,_) -> String.compare s1 s2) (loop 0) in
	all,!m