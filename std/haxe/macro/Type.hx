/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package haxe.macro;

typedef Ref<T> = {
	public function get() : T;
	public function toString() : String;
}

enum Type {
	TMono( t : Ref<Null<Type>> );
	TEnum( t : Ref<EnumType>, params : Array<Type> );
	TInst( t : Ref<ClassType>, params : Array<Type> );
	TType( t : Ref<DefType>, params : Array<Type> );
	TFun( args : Array<{ name : String, opt : Bool, t : Type }>, ret : Type );
	TAnonymous( a : Ref<AnonType> );
	TDynamic( t : Null<Type> );
	TLazy( f : Void -> Type );
	TAbstract( t : Ref<AbstractType>, params : Array<Type> );
}

typedef AnonType = {
	var fields : Array<ClassField>;
	var status : AnonStatus;
}

enum AnonStatus {
	AClosed;
	AOpened;
	AConst;
	AExtend( tl:Ref<Array<Type>> );
	AClassStatics( t : Ref<ClassType> );
	AEnumStatics( t : Ref<EnumType> );
	AAbstractStatics( t : Ref<AbstractType> );
}

typedef TypeParameter = {
	var name: String;
	var t: Type;
}

typedef BaseType = {
	var pack : Array<String>;
	var name : String;
	var module : String;
	var pos : Expr.Position;
	var isPrivate : Bool;
	var isExtern : Bool;
	var params : Array<TypeParameter>;
	var meta : MetaAccess;
	var doc : Null<String>;
	function exclude() : Void;
}

typedef ClassField = {
	var name : String;
	var type : Type;
	var isPublic : Bool;
	var params : Array<TypeParameter>;
	var meta : MetaAccess;
	var kind : FieldKind;
	function expr() : Null<TypedExpr>;
	var pos : Expr.Position;
	var doc : Null<String>;
}

enum ClassKind {
	KNormal;
	KTypeParameter(constraints:Array<Type>);
	KExtension(cl:Ref<ClassType>, params:Array<Type>);
	KExpr(expr:Expr);
	KGeneric;
	KGenericInstance(cl:Ref<ClassType>, params:Array<Type>);
	KMacroType;
	KAbstractImpl(a:Ref<AbstractType>);
	KGenericBuild;
}

typedef ClassType = {> BaseType,
	var kind : ClassKind;
	var isInterface : Bool;
	var superClass : Null<{ t : Ref<ClassType>, params : Array<Type> }>;
	var interfaces : Array<{ t : Ref<ClassType>, params : Array<Type> }>;
	var fields : Ref<Array<ClassField>>;
	var statics : Ref<Array<ClassField>>;
	//var dynamic : Null<Type>;
	//var arrayAccess : Null<Type>;
	var constructor : Null<Ref<ClassField>>;
	var init : Null<TypedExpr>;
	var overrides : Array<Ref<ClassField>>;
}

typedef EnumField = {
	var name : String;
	var type : Type;
	var pos : Expr.Position;
	var meta : MetaAccess;
	var index : Int;
	var doc : Null<String>;
	var params : Array<TypeParameter>;
}

typedef EnumType = {> BaseType,
	var constructs : haxe.ds.StringMap<EnumField>;
	var names : Array<String>;
}

typedef DefType = {> BaseType,
	var type : Type;
}

typedef AbstractType = {>BaseType,
	var type : Type;
	var impl : Null<Ref<ClassType>>;
	var binops : Array<{op:Expr.Binop, field:ClassField}>;
	var unops : Array<{op:Expr.Unop, postFix:Bool, field:ClassField}>;
	var from : Array<{t:Type, field:Null<ClassField>}>;
	var to : Array<{t:Type, field:Null<ClassField>}>;
	var array : Array<ClassField>;
}

/**
	MetaAccess is a wrapper for the `Metadata` array. It can be used to add
	metadata to and remove metadata from its origin.
**/
typedef MetaAccess = {
	/**
		Return the wrapped `Metadata` array.

		Modifying this array has no effect on the origin of `this` MetaAccess.
		The `add` and `remove` methods can be used for that.
	**/
	function get() : Expr.Metadata;

	/**
		Adds the metadata specified by `name`, `params` and `pos` to the origin
		of `this` MetaAccess.

		Metadata names are not unique during compilation, so this method never
		overwrites a previous metadata.

		If a `Metadata` array is obtained through a call to `get`, a subsequent
		call to `add` has no effect on that array.

		If any argument is null, compilation fails with an error.
	**/
	function add( name : String, params : Array<Expr>, pos : Expr.Position ) : Void;

	/**
		Removes all `name` metadata entries from the origin of `this`
		MetaAccess.

		This method might clear several metadata entries of the same name.

		If a `Metadata` array is obtained through a call to `get`, a subsequent
		call to `remove` has no effect on that array.

		If `name` is null, compilation fails with an error.
	**/
	function remove( name : String ) : Void;

	/**
		Tells if the origin of `this` MetaAccess has a `name` metadata entry.

		If `name` is null, compilation fails with an error.
	**/
	function has( name : String ) : Bool;
}

enum FieldKind {
	FVar( read : VarAccess, write : VarAccess );
	FMethod( k : MethodKind );
}

enum VarAccess {
	AccNormal;
	AccNo;
	AccNever;
	AccResolve;
	AccCall;
	AccInline;
	AccRequire( r : String, ?msg : String );
}

enum MethodKind {
	MethNormal;
	MethInline;
	MethDynamic;
	MethMacro;
}

enum TConstant {
	TInt(i:Int);
	TFloat(s:String);
	TString(s:String);
	TBool(b:Bool);
	TNull;
	TThis;
	TSuper;
}

typedef TVar = {
	id: Int,
	name: String,
	t: Type,
	capture: Bool,
	extra: Null<{params: Array<TypeParameter>, expr: Null<TypedExpr>}>
}

enum ModuleType {
	TClassDecl(c:Ref<ClassType>);
	TEnumDecl(e:Ref<EnumType>);
	TTypeDecl(t:Ref<DefType>);
	TAbstract(a:Ref<AbstractType>);
}

typedef TFunc = {
	args: Array<{v:TVar, value:Null<TConstant>}>,
	t: Type,
	expr: TypedExpr
}

enum FieldAccess {
	FInstance(c:Ref<ClassType>, cf:Ref<ClassField>);
	FStatic(c:Ref<ClassType>, cf:Ref<ClassField>);
	FAnon(cf:Ref<ClassField>);
	FDynamic(s:String);
	FClosure(c:Null<Ref<ClassType>>, cf:Ref<ClassField>);
	FEnum(e:Ref<EnumType>, ef:EnumField);
}

enum TypedExprDef {
	TConst(c:TConstant);
	TLocal(v:TVar);
	TArray(e1:TypedExpr, e2:TypedExpr);
	TBinop(op:Expr.Binop, e1:TypedExpr, e2:TypedExpr);
	TField(e:TypedExpr, fa:FieldAccess);
	TTypeExpr(m:ModuleType);
	TParenthesis(e:TypedExpr);
	TObjectDecl(fields:Array<{name:String, expr:TypedExpr}>);
	TArrayDecl(el:Array<TypedExpr>);
	TCall(e:TypedExpr, el:Array<TypedExpr>);
	TNew(c:Ref<ClassType>, params: Array<Type>, el:Array<TypedExpr>);
	TUnop(op:Expr.Unop, postFix:Bool, e:TypedExpr);
	TFunction(tfunc:TFunc);
	TVar(v:TVar, expr:Null<TypedExpr>);
	TBlock(el:Array<TypedExpr>);
	TFor(v:TVar, e1:TypedExpr, e2:TypedExpr);
	TIf(econd:TypedExpr, eif:TypedExpr, eelse:Null<TypedExpr>);
	TWhile(econd:TypedExpr, e:TypedExpr, normalWhile:Bool);
	TSwitch(e:TypedExpr, cases:Array<{values:Array<TypedExpr>, expr:TypedExpr}>, edef:Null<TypedExpr>);
	TPatMatch;
	TTry(e:TypedExpr, catches:Array<{v:TVar, expr:TypedExpr}>);
	TReturn(e:Null<TypedExpr>);
	TBreak;
	TContinue;
	TThrow(e:TypedExpr);
	TCast(e:TypedExpr, m:Null<ModuleType>);
	TMeta(m:Expr.MetadataEntry, e1:TypedExpr);
	TEnumParameter(e1:TypedExpr, ef:EnumField, index:Int);
}

typedef TypedExpr = {
	expr: TypedExprDef,
	pos: Expr.Position,
	t: Type
}
