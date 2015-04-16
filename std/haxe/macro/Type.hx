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

/*
    Warning: Some of these types correspond to compiler-internal data structures
    and might change in minor Haxe releases in order to adapt to internal changes.
*/

/**
	Represents a reference to internal compiler structure. It exists because
	encoding compiler structures for use in macros is quite expensive. 
	A structure is only encoded when user requests it through `ref.get()`.
*/
typedef Ref<T> = {
	public function get() : T;
	public function toString() : String;
}

/**
	Represents a type 
*/
enum Type {
	/**
		Represents a monomorph.

		@see http://haxe.org/manual/types-monomorph.html
	**/
	TMono( t : Ref<Null<Type>> );

	/**
		Represents an enum instance.

		@see http://haxe.org/manual/types-enum-instance.html
	**/
	TEnum( t : Ref<EnumType>, params : Array<Type> );

	/**
		Represents a class instance.

		@see http://haxe.org/manual/types-class-instance.html
	**/
	TInst( t : Ref<ClassType>, params : Array<Type> );

	/**
		Represents a typedef.

		@see http://haxe.org/manual/type-system-typedef.html
	**/
	TType( t : Ref<DefType>, params : Array<Type> );

	/**
		Represents a function type.

		@see http://haxe.org/manual/types-function.html
	**/
	TFun( args : Array<{ name : String, opt : Bool, t : Type }>, ret : Type );

	/**
		Represents an anonymous structure type.

		@see http://haxe.org/manual/types-anonymous-structure.html
	**/
	TAnonymous( a : Ref<AnonType> );

	/**
		Represents Dynamic.

		@see http://haxe.org/manual/types-dynamic.html
	**/
	TDynamic( t : Null<Type> );

	/**
		Used internally by the compiler to delay some typing.
	**/
	TLazy( f : Void -> Type );

	/**
		Represents an abstract type.

		@see http://haxe.org/manual/types-abstract.html
	**/
	TAbstract( t : Ref<AbstractType>, params : Array<Type> );
}

/**
	Represents an anonymous type 
*/
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

/**
	Represents type parameters 
*/
typedef TypeParameter = {
	var name: String;
	var t: Type;
}

/**
	Represents a base type 
*/
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

/**
	Represents a class field 
*/
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

/**
	Represents a class kind 
*/
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

/**
	Represents a class type 
*/
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

/**
	Represents an enum field 
*/
typedef EnumField = {
	var name : String;
	var type : Type;
	var pos : Expr.Position;
	var meta : MetaAccess;
	var index : Int;
	var doc : Null<String>;
	var params : Array<TypeParameter>;
}

/**
	Represents an enum type 
*/
typedef EnumType = {> BaseType,
	var constructs : Map<String,EnumField>;
	var names : Array<String>;
}

/**
	Represents a typedef 
*/
typedef DefType = {> BaseType,
	var type : Type;
}

/**
	Represents an abstract type 
*/
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
		Extract metadata entries by given `name`.

		If there's no metadata with such name, empty array is returned.

		If `name` is null, compilation fails with an error.
	**/
	function extract( name : String ) : Array<Expr.MetadataEntry>;

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

/**
	Represents a field kind 
*/
enum FieldKind {
	FVar( read : VarAccess, write : VarAccess );
	FMethod( k : MethodKind );
}

/**
	Represents the variable accessor 
*/
enum VarAccess {
	AccNormal;
	AccNo;
	AccNever;
	AccResolve;
	AccCall;
	AccInline;
	AccRequire( r : String, ?msg : String );
}

/**
	Represents the method kind 
*/
enum MethodKind {
	MethNormal;
	MethInline;
	MethDynamic;
	MethMacro;
}

/**
	Represents typed constant 
*/
enum TConstant {
	TInt(i:Int);
	TFloat(s:String);
	TString(s:String);
	TBool(b:Bool);
	TNull;
	TThis;
	TSuper;
}

/**
	Represents type variable 
*/
typedef TVar = {
	public var id(default, never):Int;
	public var name(default, never):String;
	public var t(default, never):Type;
	public var capture(default, never):Bool;
	public var extra(default,never):Null<{params: Array<TypeParameter>, expr: Null<TypedExpr>}>;
}

/**
	Represents a type module 
*/
enum ModuleType {
	TClassDecl(c:Ref<ClassType>);
	TEnumDecl(e:Ref<EnumType>);
	TTypeDecl(t:Ref<DefType>);
	TAbstract(a:Ref<AbstractType>);
}

/**
	Represents a type function 
*/
typedef TFunc = {
	args: Array<{v:TVar, value:Null<TConstant>}>,
	t: Type,
	expr: TypedExpr
}

/**
	Represents the field accessor 
*/
enum FieldAccess {
	FInstance(c:Ref<ClassType>, params:Array<Type>, cf:Ref<ClassField>);
	FStatic(c:Ref<ClassType>, cf:Ref<ClassField>);
	FAnon(cf:Ref<ClassField>);
	FDynamic(s:String);
	FClosure(c:Null<{c:Ref<ClassType>, params:Array<Type>}>, cf:Ref<ClassField>);
	FEnum(e:Ref<EnumType>, ef:EnumField);
}

/**
	Represents a typed expression definition 
*/
enum TypedExprDef {
	/**
		Represents a constant definition.
	**/
	TConst(c:TConstant);
	/**
		Represents a local definition.
	**/
	TLocal(v:TVar);
	/**
		Represents an array definition.
	**/
	TArray(e1:TypedExpr, e2:TypedExpr);
	/**
		Represents an operator.
	**/
	TBinop(op:Expr.Binop, e1:TypedExpr, e2:TypedExpr);
	/**
		Represents a field.
	**/
	TField(e:TypedExpr, fa:FieldAccess);
	/**
		Represents a type expression.
	**/
	TTypeExpr(m:ModuleType);
	/**
		Represents a parenthesis.
	**/
	TParenthesis(e:TypedExpr);
	/**
		Represents an object declaration.
	**/
	TObjectDecl(fields:Array<{name:String, expr:TypedExpr}>);
	/**
		Represents an array declaration.
	**/
	TArrayDecl(el:Array<TypedExpr>);
	/**
		Represents a function call declaration.
	**/
	TCall(e:TypedExpr, el:Array<TypedExpr>);
	/**
		Represents a `new` function statement declaration with 
		related params.
	**/
	TNew(c:Ref<ClassType>, params: Array<Type>, el:Array<TypedExpr>);
	/**
		Represents an unary operation declaration.
	**/
	TUnop(op:Expr.Unop, postFix:Bool, e:TypedExpr);
	/**
		Represents a `function` declaration.
	**/
	TFunction(tfunc:TFunc);
	/**
		Represents a `var` declaration.
	**/
	TVar(v:TVar, expr:Null<TypedExpr>);
	/**
		Represents a block declaration `{}`.
	**/
	TBlock(el:Array<TypedExpr>);
	/**
		Represents a `for`-statement declaration.
	**/
	TFor(v:TVar, e1:TypedExpr, e2:TypedExpr);
	/**
		Represents a condition declaration.
	**/
	TIf(econd:TypedExpr, eif:TypedExpr, eelse:Null<TypedExpr>);
	/**
		Represents a `while`-statement declaration. 
		When `normalWhile` is `true` it's `while (...)`
		When `normalWhile` is `false` it's `do {...} while (...)`
	**/
	TWhile(econd:TypedExpr, e:TypedExpr, normalWhile:Bool);
	/**
		Represents a `switch`-statement declaration with related 
		cases.
	**/
	TSwitch(e:TypedExpr, cases:Array<{values:Array<TypedExpr>, expr:TypedExpr}>, edef:Null<TypedExpr>);
	/**
		Represents a `try`-statement declaration with related 
		catches.
	**/
	TTry(e:TypedExpr, catches:Array<{v:TVar, expr:TypedExpr}>);
	/**
		Represents a `return`-statement declaration. 
	**/
	TReturn(e:Null<TypedExpr>);
	/**
		Represents a `break`-statement declaration.
	**/
	TBreak;
	/**
		Represents a `continue`-statement declaration.
	**/
	TContinue;
	/**
		Represents a `throw`-statement declaration.
	**/
	TThrow(e:TypedExpr);
	/**
		Represents a `cast` declaration.
	**/
	TCast(e:TypedExpr, m:Null<ModuleType>);
	/**
		Represents a metadata declaration.
	**/
	TMeta(m:Expr.MetadataEntry, e1:TypedExpr);
	/**
		Represents an enum parameter declaration.
	**/
	TEnumParameter(e1:TypedExpr, ef:EnumField, index:Int);
}

/**
	Represents expression typed for specified target
*/
typedef TypedExpr = {
	expr: TypedExprDef,
	pos: Expr.Position,
	t: Type
}
