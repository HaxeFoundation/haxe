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

typedef JsonTodo = Dynamic;

typedef JsonPos = {
	var file:String;
	var min:Int;
	var max:Int;
}

typedef JsonDoc = Null<String>;

enum abstract ImportStatus(Int) {
	/**
		This type is already available with it's unqualified name for one of these reasons:
		  - it's a toplevel type
		  - it's imported with an `import` in the current module
		  - it's imported in an `import.hx` file
	**/
	var Imported;

	/**
		The type is currently not imported. It can be accessed either
		with its fully qualified name or by inserting an import.
	**/
	var Unimported;

	/**
		A type with the same name is already imported in the module.
		The fully qualified name has to be used to access it.
	**/
	var Shadowed;
}

/* Type instance */
typedef JsonPackagePath = {
	var pack:Array<String>;
}

typedef JsonModulePath = JsonPackagePath & {
	var moduleName:String;
	var ?importStatus:ImportStatus;
}

typedef JsonTypePath = JsonModulePath & {
	var typeName:String;
}

typedef JsonStaticFieldPath = JsonTypePath & {
	var fieldName:String;
}

typedef JsonTypePathWithParams = {
	var path:JsonTypePath;
	var params:JsonTypes;
}

typedef JsonFunctionArgument = {
	var name:String;
	var opt:Bool;
	var t:JsonType<Dynamic>;
	var ?value:{
		var string:String;
	};
}

typedef JsonFunctionSignature = {
	var args:Array<JsonFunctionArgument>;
	var ret:JsonType<Dynamic>;
}

enum abstract JsonAnonStatusKind<T>(String) {
	var AClosed;
	var AOpened;
	var AConst;
	var AExtend:JsonAnonStatusKind<JsonTypes>;
	var AClassStatics:JsonAnonStatusKind<JsonTypePath>;
	var AEnumStatics:JsonAnonStatusKind<JsonTypePath>;
	var AAbstractStatics:JsonAnonStatusKind<JsonTypePath>;
}

typedef JsonAnonStatus<T> = {
	var kind:JsonAnonStatusKind<T>;
	var args:T;
}

typedef JsonAnon = {
	var fields:JsonClassFields;
	var status:JsonAnonStatus<Dynamic>;
}

enum abstract JsonTypeKind<T>(String) {
	var TMono;
	var TInst:JsonTypeKind<JsonTypePathWithParams>;
	var TEnum:JsonTypeKind<JsonTypePathWithParams>;
	var TType:JsonTypeKind<JsonTypePathWithParams>;
	var TAbstract:JsonTypeKind<JsonTypePathWithParams>;
	var TFun:JsonTypeKind<JsonFunctionSignature>;
	var TAnonymous:JsonTypeKind<JsonAnon>;
	var TDynamic:JsonTypeKind<Null<JsonType<Dynamic>>>;
}

typedef JsonType<T> = {
	var kind:JsonTypeKind<T>;
	var args:T;
}

typedef JsonTypes = Array<JsonType<Dynamic>>;

/* Type parameters */
typedef JsonTypeParameter = {
	var name:String;
	var constraints:JsonTypes;
}

typedef JsonTypeParameters = Array<JsonTypeParameter>;

/* Expr */
enum abstract JsonBinopKind<T>(String) {
	var OpAdd;
	var OpMult;
	var OpDiv;
	var OpSub;
	var OpAssign;
	var OpEq;
	var OpNotEq;
	var OpGt;
	var OpGte;
	var OpLt;
	var OpLte;
	var OpAnd;
	var OpOr;
	var OpXor;
	var OpBoolAnd;
	var OpBoolOr;
	var OpShl;
	var OpShr;
	var OpUShr;
	var OpMod;
	var OpAssignOp:JsonBinopKind<JsonBinop<Dynamic>>;
	var OpInterval;
	var OpArrow;
	var OpIn;
	var OpNullCoal;
}

typedef JsonBinop<T> = {
	var kind:JsonBinopKind<T>;
	var args:T;
}

enum abstract JsonUnop(String) {
	var OpIncrement;
	var OpDecrement;
	var OpNot;
	var OpNeg;
	var OpNegBits;
}

typedef JsonExpr = JsonTodo;

typedef JsonMetadataEntry = {
	var name:String;
	var args:Array<JsonExpr>;
	var pos:JsonPos;
}

typedef JsonMetadata = Array<JsonMetadataEntry>;

enum abstract JsonTConstantKind<T>(String) {
	var TInt:JsonTConstantKind<String>;
	var TFloat:JsonTConstantKind<String>;
	var TString:JsonTConstantKind<String>;
	var TBool:JsonTConstantKind<Bool>;
	var TNull;
	var TThis;
	var TSuper;
}

typedef JsonTConstant<T> = {
	var kind:JsonTConstantKind<T>;
	var args:T;
}

typedef JsonTExpr = JsonTodo;

/* Fields */
enum abstract JsonVarAccessKind<T>(String) {
	var AccNormal;
	var AccNo;
	var AccNever;
	var AccResolve;
	var AccCall;
	var AccInline;
	var AccRequire:JsonVarAccessKind<{require:String, message:Null<String>}>;
	var AccCtor;
}

typedef JsonVarAccess<T> = {
	var kind:JsonVarAccessKind<T>;
	var args:T;
}

enum abstract JsonMethodKind(String) {
	var MethNormal;
	var MethInline;
	var MethDynamic;
	var MethMacro;
}

enum abstract JsonFieldKindKind<T>(String) {
	var FVar:JsonFieldKindKind<{read:JsonVarAccess<Dynamic>, write:JsonVarAccess<Dynamic>}>;
	var FMethod:JsonFieldKindKind<JsonMethodKind>;
}

typedef JsonFieldKind<T> = {
	var kind:JsonFieldKindKind<T>;
	var args:T;
}

enum abstract JsonClassFieldScope(Int) {
	var Static;
	var Member;
	var Constructor;
}

typedef JsonClassField = {
	var name:String;
	var type:JsonType<Dynamic>;
	var isPublic:Bool;
	var isFinal:Bool;
	var isAbstract:Bool;
	var params:JsonTypeParameters;
	var meta:JsonMetadata;
	var kind:JsonFieldKind<Dynamic>;
	var ?expr:{
		var string:String;
	};
	var pos:JsonPos;
	var doc:JsonDoc;
	var overloads:JsonClassFields;
	var scope:JsonClassFieldScope;
}

typedef JsonClassFields = Array<JsonClassField>;
typedef JsonClassFieldReference = String;

typedef JsonEnumField = {
	var name:String;
	var type:JsonType<Dynamic>;
	var pos:JsonPos;
	var meta:JsonMetadata;
	var index:Int;
	var doc:JsonDoc;
	var params:JsonTypeParameters;
}

typedef JsonEnumFields = Array<JsonEnumField>;

/* Class */
enum abstract JsonClassKindKind<T>(String) {
	var KNormal;
	var KTypeParameter:JsonClassKindKind<JsonTypes>;
	var KExtension:JsonClassKindKind<JsonTypePathWithParams>;
	var KExpr:JsonClassKindKind<JsonExpr>;
	var KGeneric;
	var KGenericInstance:JsonClassKindKind<JsonTypePathWithParams>;
	var KMacroType;
	var KAbstractImpl:JsonClassKindKind<JsonTypePath>;
	var KGenericBuild;
	var KModuleFields:JsonClassKindKind<JsonModulePath>;
}

typedef JsonClassKind<T> = {
	var kind:JsonClassKindKind<T>;
	var args:T;
}

typedef JsonClass = {
	var kind:JsonClassKind<Dynamic>;
	var isInterface:Bool;
	var isExtern:Bool;
	var isFinal:Bool;
	var isAbstract:Bool;
	var superClass:Null<JsonTypePathWithParams>;
	var interfaces:Array<JsonTypePathWithParams>;
	var fields:JsonClassFields;
	var statics:JsonClassFields;
	var constructor:Null<JsonClassField>;
	var init:Null<JsonTExpr>;
	var overrides:Array<JsonClassFieldReference>;
}

/* Enum */
typedef JsonEnum = {
	var constructors:JsonEnumFields;
	var isExtern:Bool;
}

/* Typedef */
typedef JsonTypedef = {
	var type:JsonType<Dynamic>;
}

/* Abstract */
typedef JsonAbstractBinop = {
	var op:JsonBinop<Dynamic>;
	var field:JsonClassFieldReference;
}

typedef JsonAbstractUnop = {
	var op:JsonUnop;
	var postFix:Bool;
	var field:JsonClassFieldReference;
}

typedef JsonAbstractCast = {
	var t:JsonType<Dynamic>;
	var field:JsonClassFieldReference;
}

typedef JsonAbstract = {
	var type:JsonType<Dynamic>;
	var impl:Null<JsonClass>;
	var binops:Array<JsonAbstractBinop>;
	var unops:Array<JsonAbstractUnop>;
	var from:Array<JsonAbstractCast>;
	var to:Array<JsonAbstractCast>;
	var array:JsonClassFields;
	var resolve:Null<JsonClassFieldReference>;
}

/* Module type */
enum abstract JsonModuleTypeKind<T>(String) {
	var Class:JsonModuleTypeKind<JsonClass> = "class";
	var Enum:JsonModuleTypeKind<JsonEnum> = "enum";
	var Typedef:JsonModuleTypeKind<JsonTypedef> = "typedef";
	var Abstract:JsonModuleTypeKind<JsonAbstract> = "abstract";
}

typedef JsonModuleType<T> = {
	var pack:Array<String>;
	var name:String;
	var moduleName:String;
	var pos:JsonPos;
	var isPrivate:Bool;
	var params:JsonTypeParameters;
	var meta:JsonMetadata;
	var doc:JsonDoc;
	var kind:JsonModuleTypeKind<T>;
	var args:T;
}

typedef JsonModuleTypes = Array<JsonModuleType<Dynamic>>;
