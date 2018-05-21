/*
 * Copyright (C)2005-2018 Haxe Foundation
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

/* Type instance */

typedef JsonPath = {
	var pack: Array<String>;
	var name: String;
}

typedef JsonPathWithParams = {
	var path: JsonPath;
	var params: JsonTypes;
}

typedef JsonFunctionArgument = {
	var name: String;
	var opt: Bool;
	var t: JsonType<Dynamic>;
}

typedef JsonFunctionSignature = {
	var args: Array<JsonFunctionArgument>;
	var ret: JsonType<Dynamic>;
}

enum abstract JsonAnonStatusKind<T>(String) {
	var AClosed = "AClosed";
	var AOpened = "AOpened";
	var AConst = "AConst";
	var AExtend:JsonAnonStatusKind<JsonTypes> = "AExtend";
	var AClassStatics:JsonAnonStatusKind<JsonPath> = "AClassStatics";
	var AEnumStatics:JsonAnonStatusKind<JsonPath> = "AEnumStatics";
	var AAbstractStatics:JsonAnonStatusKind<JsonPath> = "AAbstractStatics";
}

typedef JsonAnonStatus<T> = {
	var kind: JsonAnonStatusKind<T>;
	var args: T;
}

typedef JsonAnon = {
	var fields: JsonClassFields;
	var status: JsonAnonStatus<Dynamic>;
}

enum abstract JsonTypeKind<T>(String) {
	var TMono = "TMono";
	var TInst:JsonTypeKind<JsonPathWithParams> = "TInst";
	var TEnum:JsonTypeKind<JsonPathWithParams> = "TEnum";
	var TType:JsonTypeKind<JsonPathWithParams> = "TType";
	var TAbstract:JsonTypeKind<JsonPathWithParams> = "TAbstract";
	var TFun:JsonTypeKind<JsonFunctionSignature> = "TFun";
	var TAnonymous:JsonTypeKind<JsonAnon> = "TAnonymous";
	var TDynamic:JsonTypeKind<Null<JsonType<Dynamic>>> = "TDynamic";
}

typedef JsonType<T> = {
	var kind: JsonTypeKind<T>;
	var args: T;
}

typedef JsonTypes = Array<JsonType<Dynamic>>;

/* Type parameters */

typedef JsonTypeParameter = {
	var name: String;
	var constraints: JsonTypes;
}

typedef JsonTypeParameters = Array<JsonTypeParameter>;

/* Expr */

enum abstract JsonBinopKind<T>(String) {
	var OpAdd = "OpAdd";
	var OpMult = "OpMult";
	var OpDiv = "OpDiv";
	var OpSub = "OpSub";
	var OpAssign = "OpAssign";
	var OpEq = "OpEq";
	var OpNotEq = "OpNotEq";
	var OpGt = "OpGt";
	var OpGte = "OpGte";
	var OpLt = "OpLt";
	var OpLte = "OpLte";
	var OpAnd = "OpAnd";
	var OpOr = "OpOr";
	var OpXor = "OpXor";
	var OpBoolAnd = "OpBoolAnd";
	var OpBoolOr = "OpBoolOr";
	var OpShl = "OpShl";
	var OpShr = "OpShr";
	var OpUShr = "OpUShr";
	var OpMod = "OpMod";
	var OpAssignOp:JsonBinopKind<JsonBinop<Dynamic>> = "OpAssignOp";
	var OpInterval = "OpInterval";
	var OpArrow = "OpArrow";
	var OpIn = "OpIn";
}

typedef JsonBinop<T> = {
	var kind: JsonBinopKind<T>;
	var args: T;
}

enum abstract JsonUnop(String) {
	var OpIncrement = "OpIncrement";
	var OpDecrement = "OpDecrement";
	var OpNot = "OpNot";
	var OpNeg = "OpNeg";
	var OpNegBits = "OpNegBits";
}

typedef JsonExpr = JsonTodo;

typedef JsonMetadataEntry = {
	var name: String;
	var args: Array<JsonExpr>;
	var pos: JsonPos;
}

typedef JsonMetadata = Array<JsonMetadataEntry>;

typedef JsonTExpr = JsonTodo;

/* Fields */

enum abstract JsonVarAccessKind<T>(String) {
	var AccNormal = "AccNormal";
	var AccNo = "AccNo";
	var AccNever = "AccNever";
	var AccResolve = "AccResolve";
	var AccCall = "AccCall";
	var AccInline = "AccInline";
	var AccRequire:JsonVarAccessKind<{ require: String, message: Null<String> }> = "AccRequire";
	var AccCtor = "AccCtor";
}

typedef JsonVarAccess<T> = {
	var kind: JsonVarAccessKind<T>;
	var args: T;
}

enum abstract JsonMethodKind(String) {
	var MethNormal = "MethNormal";
	var MethInline = "MethInline";
	var MethDynamic = "MethDynamic";
	var MethMacro = "MethMacro";
}

enum abstract JsonFieldKindKind<T>(String) {
	var FVar:JsonFieldKindKind<{ read: JsonVarAccess<Dynamic>, write: JsonVarAccess<Dynamic> }> = "FVar";
	var FMethod:JsonFieldKindKind<JsonMethodKind> = "FMethod";
}

typedef JsonFieldKind<T> = {
	var kind: JsonFieldKindKind<T>;
	var args: T;
}

enum abstract JsonClassFieldScope(Int) {
    var Static = 0;
    var Member = 1;
    var Constructor = 2;
}

typedef JsonClassField = {
	var name: String;
	var type: JsonType<Dynamic>;
	var isPublic: Bool;
	var params: JsonTypeParameters;
	var meta: JsonMetadata;
	var kind: JsonFieldKind<Dynamic>;
	var expr: JsonTExpr;
	var pos: JsonPos;
	var doc: JsonDoc;
	var overloads: JsonClassFields;
    var scope: JsonClassFieldScope;
}

typedef JsonClassFields = Array<JsonClassField>;

typedef JsonClassFieldReference = String;

typedef JsonEnumField = {
	var name: String;
	var type: JsonType<Dynamic>;
	var pos: JsonPos;
	var meta: JsonMetadata;
	var index: Int;
	var doc: JsonDoc;
	var params: JsonTypeParameters;
}

typedef JsonEnumFields = Array<JsonEnumField>;

/* Class */

enum abstract JsonClassKindKind<T>(String) {
	var KNormal = "KNormal";
	var KTypeParameter:JsonClassKindKind<JsonTypes> = "KTypeParameter";
	var KExtension:JsonClassKindKind<JsonPathWithParams> = "KExtension";
	var KExpr:JsonClassKindKind<JsonExpr> = "KExpr";
	var KGeneric = "KGeneric";
	var KGenericInstance:JsonClassKindKind<JsonPathWithParams> = "KGenericInstance";
	var KMacroType = "KMacroType";
	var KAbstractImpl:JsonClassKindKind<JsonPath> = "KAbstractImpl";
	var KGenericBuild = "KGenericBuild";
}

typedef JsonClassKind<T> = {
	var kind:JsonClassKindKind<T>;
	var args: T;
}

typedef JsonClass = {
	var kind: JsonClassKind<Dynamic>;
	var isInterface: Bool;
	var isExtern: Bool;
	var superClass: Null<JsonPathWithParams>;
	var interfaces: Array<JsonPathWithParams>;
	var fields: JsonClassFields;
	var statics: JsonClassFields;
	var constructor: Null<JsonClassField>;
	var init: Null<JsonTExpr>;
	var overrides: Array<JsonClassFieldReference>;
}

/* Enum */

typedef JsonEnum = {
	var constructors: JsonEnumFields;
	var isExtern: Bool;
}

/* Typedef */

typedef JsonTypedef = {
	var type: JsonType<Dynamic>;
}

/* Abstract */

typedef JsonAbstractBinop = {
	var op: JsonBinop<Dynamic>;
	var field: JsonClassFieldReference;
}

typedef JsonAbstractUnop = {
	var op: JsonUnop;
	var postFix: Bool;
	var field: JsonClassFieldReference;
}

typedef JsonAbstractCast = {
	var t:JsonType<Dynamic>;
	var field: JsonClassFieldReference;
}

typedef JsonAbstract = {
	var type: JsonType<Dynamic>;
	var impl: Null<JsonPath>;
	var binops: Array<JsonAbstractBinop>;
	var unops: Array<JsonAbstractUnop>;
	var from: Array<JsonAbstractCast>;
	var to: Array<JsonAbstractCast>;
	var array: JsonClassFields;
	var resolve: Null<JsonClassFieldReference>;
}

/* Module type */

enum abstract JsonModuleTypeKind<T>(String) {
	var Class:JsonModuleTypeKind<JsonClass> = "class";
	var Enum:JsonModuleTypeKind<JsonEnum> = "enum";
	var Typedef:JsonModuleTypeKind<JsonTypedef> = "typedef";
	var Abstract:JsonModuleTypeKind<JsonAbstract> = "abstract";
}

typedef JsonModuleType<T> = {
	var pack: Array<String>;
	var name: String;
	var moduleName: String;
	var pos: JsonPos;
	var isPrivate: Bool;
	var params: JsonTypeParameters;
	var meta: JsonMetadata;
	var doc: JsonDoc;
	var kind: JsonModuleTypeKind<T>;
	var args: T;
}

typedef JsonModuleTypes = Array<JsonModuleType<Dynamic>>;