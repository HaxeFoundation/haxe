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
	//var status : AnonStatus;
}

typedef BaseType = {
	var pack : Array<String>;
	var name : String;
	var module : String;
	var pos : Expr.Position;
	var isPrivate : Bool;
	var isExtern : Bool;
	var params : Array<{ name : String, t : Type }>;
	var meta : MetaAccess;
	var doc : Null<String>;
	function exclude() : Void;
}

typedef ClassField = {
	var name : String;
	var type : Type;
	var isPublic : Bool;
	var params : Array<{ name : String, t : Type }>;
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
}

typedef EnumField = {
	var name : String;
	var type : Type;
	var pos : Expr.Position;
	var meta : MetaAccess;
	var index : Int;
	var doc : Null<String>;
	var params : Array<{ name : String, t : Type }>;
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

extern enum TypedExpr {}