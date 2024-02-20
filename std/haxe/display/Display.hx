/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package haxe.display;

import haxe.display.JsonModuleTypes;
import haxe.display.Position;
import haxe.display.Protocol;
import haxe.ds.ReadOnlyArray;

/**
	Methods of the JSON-RPC-based `--display` protocol in Haxe 4.
	A lot of the methods are *inspired* by the Language Server Protocol, but there is **no** intention to be directly compatible with it.
**/
@:publicFields
class DisplayMethods {
	/**
		TODO documentation
	**/
	static inline var Diagnostics = new HaxeRequestMethod<DiagnosticsParams, DiagnosticsResult>("display/diagnostics");

	/**
		The completion request is sent from the client to Haxe to request code completion.
		Haxe automatically determines the type of completion to use based on the passed position, see `CompletionResultKind`.
	**/
	static inline var Completion = new HaxeRequestMethod<CompletionParams, CompletionResult>("display/completion");

	/**
		The request is sent from the client to Haxe to resolve additional information for a given completion item.
	**/
	static inline var CompletionItemResolve = new HaxeRequestMethod<CompletionItemResolveParams, CompletionItemResolveResult>("display/completionItem/resolve");

	/**
		The find references request is sent from the client to Haxe to find locations that reference the symbol at a given text document position.
	**/
	static inline var FindReferences = new HaxeRequestMethod<FindReferencesParams, GotoDefinitionResult>("display/references");

	/**
		The goto definition request is sent from the client to Haxe to resolve the definition location(s) of a symbol at a given text document position.
	**/
	static inline var GotoDefinition = new HaxeRequestMethod<PositionParams, GotoDefinitionResult>("display/definition");

	/**
		The goto implementation request is sent from the client to Haxe to resolve the implementation location(s) of a symbol at a given text document position.
	**/
	static inline var GotoImplementation = new HaxeRequestMethod<PositionParams, GotoDefinitionResult>("display/implementation");

	/**
		The goto type definition request is sent from the client to Haxe to resolve the type definition location(s) of a symbol at a given text document position.
	**/
	static inline var GotoTypeDefinition = new HaxeRequestMethod<PositionParams, GotoTypeDefinitionResult>("display/typeDefinition");

	/**
		The hover request is sent from the client to Haxe to request hover information at a given text document position.
	**/
	static inline var Hover = new HaxeRequestMethod<PositionParams, HoverResult>("display/hover");

	/**
		This request is sent from the client to Haxe to determine the package for a given file, based on class paths configuration.
	**/
	static inline var DeterminePackage = new HaxeRequestMethod<FileParams, DeterminePackageResult>("display/package");

	/**
		The signature help request is sent from the client to Haxe to request signature information at a given cursor position.
	**/
	static inline var SignatureHelp = new HaxeRequestMethod<SignatureHelpParams, SignatureHelpResult>("display/signatureHelp");

	/**
		The metadata request is sent from the client to Haxe to get a list of all registered metadata and their documentation.
	**/
	static inline var Metadata = new HaxeRequestMethod<MetadataParams, MetadataResult>("display/metadata");

	/**
		The defines request is sent from the client to Haxe to get a list of all registered defines and their documentation.
	**/
	static inline var Defines = new HaxeRequestMethod<DefinesParams, DefinesResult>("display/defines");

	/*
		TODO:

		- finish completion
		- codeLens
		- workspaceSymbols ("project/symbol"?)
	 */
}

/** Completion **/
typedef CompletionParams = PositionParams & {
	var wasAutoTriggered:Bool;

	/** list of metas to include in responses **/
	var ?meta:Array<String>;
}

typedef FieldResolution = {
	/**
		Whether it's valid to use the unqualified name of the field or not.
		This is `false` if the identifier is shadowed.
	**/
	var isQualified:Bool;

	/**
		The qualifier that has to be inserted to use the field if `!isQualified`.
		Can either be `this` or `super` for instance fields for the type name for `static` fields.
	**/
	var qualifier:String;
}

typedef DisplayLocal<T> = {
	var id:Int;
	var name:String;
	var type:JsonType<T>;
	var origin:LocalOrigin;
	var capture:Bool;
	var ?extra:{
		var params:Array<JsonTypeParameter>;
		var expr:JsonExpr;
	};
	var meta:JsonMetadata;
	var pos:JsonPos;
	var isInline:Bool;
	var isFinal:Bool;
}

enum abstract LocalOrigin(Int) {
	var LocalVariable;
	var Argument;
	var ForVariable;
	var PatternVariable;
	var CatchVariable;
	var LocalFunction;
}

enum abstract ClassFieldOriginKind<T>(Int) {
	/**
		The field is declared on the current type itself.
	**/
	var Self:ClassFieldOriginKind<JsonModuleType<T>>;

	/**
		The field is a static field brought into context via a static import
		(`import pack.Module.Type.field`).
	**/
	var StaticImport:ClassFieldOriginKind<JsonModuleType<T>>;

	/**
		The field is declared on a parent type, such as:
		- a super class field that is not overriden
		- a forwarded abstract field
	**/
	var Parent:ClassFieldOriginKind<JsonModuleType<T>>;

	/**
		The field is a static extension method brought
		into context with the `using` keyword.
	**/
	var StaticExtension:ClassFieldOriginKind<JsonModuleType<T>>;

	/**
		This field doesn't belong to any named type, just an anonymous structure.
	**/
	var AnonymousStructure:ClassFieldOriginKind<JsonAnon>;

	/**
		Special fields built into the compiler, such as:
		- `code` on single-character Strings
		- `bind()` on functions.
	**/
	var BuiltIn:ClassFieldOriginKind<NoData>;

	/**
		The origin of this class field is unknown.
	**/
	var Unknown:ClassFieldOriginKind<NoData>;
}

typedef ClassFieldOrigin<T> = {
	var kind:ClassFieldOriginKind<T>;
	var ?args:T;
}

typedef ClassFieldOccurrence<T> = {
	var field:JsonClassField;
	var resolution:FieldResolution;
	var ?origin:ClassFieldOrigin<T>;
}

enum abstract EnumFieldOriginKind<T>(Int) {
	/**
		The enum value is declared on the current type itself.
	**/
	var Self:EnumFieldOriginKind<JsonModuleType<T>>;

	/**
		The enum value is brought into context via a static import
		(`import pack.Module.Enum.Value`).
	**/
	var StaticImport:EnumFieldOriginKind<JsonModuleType<T>>;
}

typedef EnumFieldOrigin<T> = {
	var kind:EnumFieldOriginKind<T>;
	var ?args:T;
}

typedef EnumFieldOccurrence<T> = {
	var field:JsonEnumField;
	var resolution:FieldResolution;
	var ?origin:EnumFieldOrigin<T>;
}

enum abstract Literal(String) {
	var Null = "null";
	var True = "true";
	var False = "false";
	var This = "this";
	var Trace = "trace";
}

enum abstract DisplayModuleTypeKind(Int) {
	var Class;
	var Interface;
	var Enum;
	var Abstract;
	var EnumAbstract;

	/** A `typedef` that is just an alias for another type. **/
	var TypeAlias;

	/** A `typedef` that is an alias for an anonymous structure. **/
	var Struct;

	/** A type name introduced by `import as` / `import in` **/
	// var ImportAlias;
}

typedef DisplayModuleType = {
	var path:JsonTypePath;
	var pos:JsonPos;
	var isPrivate:Bool;
	var params:Array<DisplayModuleTypeParameter>;
	var meta:JsonMetadata;
	var doc:JsonDoc;
	var isExtern:Bool;
	var isFinal:Bool;
	var isAbstract:Bool;
	var kind:DisplayModuleTypeKind;
}

typedef DisplayModuleTypeParameter = {
	var name:String;
	var meta:JsonMetadata;
	var constraints:Array<JsonType<Dynamic>>;
}

typedef DisplayLiteral<T> = {
	var name:String;
}

enum abstract MetadataTarget(String) {
	var Class = "TClass";
	var ClassField = "TClassField";
	var Abstract = "TAbstract";
	var AbstractField = "TAbstractField";
	var Enum = "TEnum";
	var Typedef = "TTypedef";
	var AnyField = "TAnyField";
	var Expr = "TExpr";
	var TypeParameter = "TTypeParameter";
}

enum abstract Platform(String) {
	var Cross = "cross";
	var Js = "js";
	var Lua = "lua";
	var Neko = "neko";
	var Flash = "flash";
	var Php = "php";
	var Cpp = "cpp";
	var Java = "java";
	var Python = "python";
	var Hl = "hl";
	var Eval = "eval";
}

typedef Metadata = {
	var name:String;
	var doc:JsonDoc;
	var parameters:Array<String>;
	var platforms:Array<Platform>;
	var targets:Array<MetadataTarget>;
	var internal:Bool;
	var ?origin:String;
	var ?links:Array<String>;
}

typedef Define = {
	var name:String;
	var value:Null<String>;
	var doc:JsonDoc;
	var parameters:Array<String>;
	var platforms:Array<Platform>;
	var links:Array<String>;
	var ?origin:String;
	var ?deprecated:String;
}

typedef Keyword = {
	var name:KeywordKind;
}

enum abstract KeywordKind(String) to String {
	var Implements = "implements";
	var Extends = "extends";
	var Function = "function";
	var Var = "var";
	var If = "if";
	var Else = "else";
	var While = "while";
	var Do = "do";
	var For = "for";
	var Break = "break";
	var Return = "return";
	var Continue = "continue";
	var Switch = "switch";
	var Case = "case";
	var Default = "default";
	var Try = "try";
	var Catch = "catch";
	var New = "new";
	var Throw = "throw";
	var Untyped = "untyped";
	var Cast = "cast";
	var Macro = "macro";
	var Package = "package";
	var Import = "import";
	var Using = "using";
	var Public = "public";
	var Private = "private";
	var Static = "static";
	var Extern = "extern";
	var Dynamic = "dynamic";
	var Override = "override";
	var Overload = "overload";
	var Class = "class";
	var Interface = "interface";
	var Enum = "enum";
	var Abstract = "abstract";
	var Typedef = "typedef";
	var Final = "final";
	var Inline = "inline";
}

/* enum abstract PackageContentKind(Int) {
	var Module;
	var Package;
}*/
typedef Package = {
	var path:JsonPackagePath;
	// var ?contents:Array<{name:String, kind:PackageContentKind}>;
}

typedef Module = {
	var path:JsonModulePath;
	// var ?contents:Array<ModuleType>;
}

enum abstract DisplayItemKind<T>(String) {
	var Local:DisplayItemKind<DisplayLocal<Dynamic>>;
	var ClassField:DisplayItemKind<ClassFieldOccurrence<Dynamic>>;
	var EnumField:DisplayItemKind<EnumFieldOccurrence<Dynamic>>;

	/** Only for the enum values in enum abstracts, other fields use `ClassField`. **/
	var EnumAbstractField:DisplayItemKind<ClassFieldOccurrence<Dynamic>>;

	var Type:DisplayItemKind<DisplayModuleType>;
	var Package:DisplayItemKind<Package>;
	var Module:DisplayItemKind<Module>;
	var Literal:DisplayItemKind<DisplayLiteral<Dynamic>>;
	var Metadata:DisplayItemKind<Metadata>;
	var Keyword:DisplayItemKind<Keyword>;
	var AnonymousStructure:DisplayItemKind<JsonAnon>;
	var Expression:DisplayItemKind<JsonTExpr>;
	var TypeParameter:DisplayItemKind<DisplayModuleTypeParameter>;
	var Define:DisplayItemKind<Define>;
}

typedef DisplayItem<T> = {
	var kind:DisplayItemKind<T>;
	var args:T;
	var ?type:JsonType<Dynamic>;
	var ?index:Int;
}

typedef DisplayItemOccurrence<T> = {
	var range:Range;
	var item:DisplayItem<T>;
	var ?moduleType:JsonModuleType<Dynamic>;
	var ?moduleTypeFollowed:JsonModuleType<Dynamic>;
}

typedef FieldCompletionSubject<T> = DisplayItemOccurrence<T> & {
	var ?iterator:{
		var type:JsonType<Dynamic>;
	};
	var ?keyValueIterator:{
		var key:JsonType<Dynamic>;
		var value:JsonType<Dynamic>;
	};
}

typedef ToplevelCompletion<T> = {
	var ?expectedType:JsonType<T>;
	var ?expectedTypeFollowed:JsonType<T>;
	var ?compatibleTypes:Array<JsonType<Dynamic>>;
}

typedef StructExtensionCompletion = {
	var isIntersectionType:Bool;
}

typedef PatternCompletion<T> = ToplevelCompletion<T> & {
	var isOutermostPattern:Bool;
}

typedef DiagnosticsParams = {
	var ?file:FsPath;
	var ?contents:String;
	var ?fileContents:Array<{file:FsPath, ?contents:String}>;
}

typedef DiagnosticsResult = Response<ReadOnlyArray<{
	var file:FsPath;
	var diagnostics:ReadOnlyArray<Diagnostic<Any>>;
}>>

enum abstract CompletionModeKind<T>(Int) {
	var Field:CompletionModeKind<FieldCompletionSubject<Dynamic>>;
	var StructureField;
	var Toplevel:CompletionModeKind<ToplevelCompletion<Dynamic>>;
	var Metadata;
	var TypeHint;
	var Extends;
	var Implements;
	var StructExtension:CompletionModeKind<StructExtensionCompletion>;
	var Import;
	var Using;
	var New;
	var Pattern:CompletionModeKind<PatternCompletion<Dynamic>>;
	var Override;
	var TypeRelation;
	var TypeDeclaration;
}

typedef CompletionMode<T> = {
	var kind:CompletionModeKind<T>;
	var ?args:T;
}

typedef CompletionResponse<T1, T2> = {
	var items:Array<DisplayItem<T1>>;
	var mode:CompletionMode<T2>;
	var ?replaceRange:Range;
	var ?isIncomplete:Bool;
	var ?filterString:String;
}

typedef CompletionResult = Response<Null<CompletionResponse<Dynamic, Dynamic>>>;

/** CompletionItem Resolve **/
typedef CompletionItemResolveParams = {
	var index:Int;
};

typedef CompletionItemResolveResult = Response<{
	var item:DisplayItem<Dynamic>;
}>;

/** FindReferences **/
typedef FindReferencesParams = PositionParams & {
	var ?kind:FindReferencesKind;
}

enum abstract FindReferencesKind(String) to String {
	/**
		Find only direct references to the requested symbol.
		Does not look for references to parent or overriding methods.
	**/
	var Direct = "direct";

	/**
		Find references to the base field and all the overidding fields in the inheritance chain.
	**/
	var WithBaseAndDescendants = "withBaseAndDescendants";

	/**
		Find references to the requested field and references to all
		descendants of the requested field.
	**/
	var WithDescendants = "withDescendants";
}

/** GotoDefinition **/
typedef GotoDefinitionResult = Response<Array<Location>>;

/** GotoTypeDefinition **/
typedef GotoTypeDefinitionResult = Response<Array<Location>>;

/** Hover **/
typedef HoverResult = Response<Null<HoverDisplayItemOccurence<Dynamic>>>;

typedef HoverDisplayItemOccurence<T> = DisplayItemOccurrence<T> & {
	var ?expected:{
		var ?type:JsonType<Dynamic>;
		var ?name:{
			var name:String;
			var kind:HoverExpectedNameKind;
			var ?doc:String;
		};
	};
}

enum abstract HoverExpectedNameKind(Int) {
	var FunctionArgument;
	var StructureField;
}

/** DeterminePackage **/
typedef DeterminePackageResult = Response<Array<String>>;

/** SignatureHelp **/
typedef SignatureHelpParams = PositionParams & {
	var wasAutoTriggered:Bool;
}

typedef SignatureInformation = JsonFunctionSignature & {
	var ?documentation:String;
}

enum abstract SignatureItemKind(Int) {
	var Call;
	var ArrayAccess;
}

typedef SignatureItem = {
	var signatures:Array<SignatureInformation>;
	var activeSignature:Int;
	var activeParameter:Int;
	var kind:SignatureItemKind;
}

typedef SignatureHelpResult = Response<Null<SignatureItem>>;

typedef MetadataParams = {
	var compiler:Bool;
	var user:Bool;
}

typedef MetadataResult = Response<Array<Metadata>>;

typedef DefinesParams = {
	var compiler:Bool;
	var user:Bool;
}

typedef DefinesResult = Response<Array<Define>>;

/** General types **/
typedef PositionParams = FileParams & {
	/** Unicode character offset in the file. **/
	var offset:Int;

	var ?contents:String;
}
