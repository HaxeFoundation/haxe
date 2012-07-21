/*
 * Copyright (c) 2005-2010, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
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
}

typedef EnumType = {> BaseType,
	var constructs : Hash<EnumField>;
	var names : Array<String>;
}

typedef DefType = {> BaseType,
	var type : Type;
}

typedef MetaAccess = {
	function get() : Expr.Metadata;
	function add( name : String, params : Array<Expr>, pos : Expr.Position ) : Void;
	function remove( name : String ) : Void;
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
	AccCall( m : String );
	AccInline;
	AccRequire( r : String );
}

enum MethodKind {
	MethNormal;
	MethInline;
	MethDynamic;
	MethMacro;
}

extern enum TypedExpr {}
