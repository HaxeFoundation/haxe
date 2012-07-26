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

#if macro
extern enum Position {
}
#else
typedef Position = {
	var file : String;
	var min : Int;
	var max : Int;
}
#end

enum Constant {
	CInt( v : String );
	CFloat( f : String );
	CString( s : String );
	CIdent( s : String );
	CRegexp( r : String, opt : String );
	#if !haxe3
	CType( s : String );
	#end
}

enum Binop {
	OpAdd;
	OpMult;
	OpDiv;
	OpSub;
	OpAssign;
	OpEq;
	OpNotEq;
	OpGt;
	OpGte;
	OpLt;
	OpLte;
	OpAnd;
	OpOr;
	OpXor;
	OpBoolAnd;
	OpBoolOr;
	OpShl;
	OpShr;
	OpUShr;
	OpMod;
	OpAssignOp( op : Binop );
	OpInterval;
}


enum Unop {
	OpIncrement;
	OpDecrement;
	OpNot;
	OpNeg;
	OpNegBits;
}

typedef Expr = {
	var expr : ExprDef;
	var pos : Position;
}

#if !haxe3
typedef ExprRequire<T> = Expr;
#end

typedef ExprOf<T> = Expr;

enum ExprDef {
	EConst( c : Constant );
	EArray( e1 : Expr, e2 : Expr );
	EBinop( op : Binop, e1 : Expr, e2 : Expr );
	EField( e : Expr, field : String );
	EParenthesis( e : Expr );
	EObjectDecl( fields : Array<{ field : String, expr : Expr }> );
	EArrayDecl( values : Array<Expr> );
	ECall( e : Expr, params : Array<Expr> );
	ENew( t : TypePath, params : Array<Expr> );
	EUnop( op : Unop, postFix : Bool, e : Expr );
	EVars( vars : Array<{ name : String, type : Null<ComplexType>, expr : Null<Expr> }> );
	EFunction( name : Null<String>, f : Function );
	EBlock( exprs : Array<Expr> );
	EFor( it : Expr, expr : Expr );
	EIn( e1 : Expr, e2 : Expr );
	EIf( econd : Expr, eif : Expr, eelse : Null<Expr> );
	EWhile( econd : Expr, e : Expr, normalWhile : Bool );
	ESwitch( e : Expr, cases : Array<{ values : Array<Expr>, expr : Expr }>, edef : Null<Expr> );
	ETry( e : Expr, catches : Array<{ name : String, type : ComplexType, expr : Expr }> );
	EReturn( ?e : Null<Expr> );
	EBreak;
	EContinue;
	EUntyped( e : Expr );
	EThrow( e : Expr );
	ECast( e : Expr, t : Null<ComplexType> );
	EDisplay( e : Expr, isCall : Bool );
	EDisplayNew( t : TypePath );
	ETernary( econd : Expr, eif : Expr, eelse : Expr );
	ECheckType( e : Expr, t : ComplexType );
	#if !haxe3
	EType( e : Expr, field : String );
	#end
}

enum ComplexType {
	TPath( p : TypePath );
	TFunction( args : Array<ComplexType>, ret : ComplexType );
	TAnonymous( fields : Array<Field> );
	TParent( t : ComplexType );
	TExtend( p : TypePath, fields : Array<Field> );
	TOptional( t : ComplexType );
}

typedef TypePath = {
	var pack : Array<String>;
	var name : String;
	var params : Array<TypeParam>;
	@:optional var sub : Null<String>;
}

enum TypeParam {
	TPType( t : ComplexType );
	TPExpr( e : Expr );
}

typedef TypeParamDecl = {
	var name : String;
	@:optional var constraints : Array<ComplexType>;
	@:optional var params : Array<TypeParamDecl>;
}

typedef Function = {
	var args : Array<FunctionArg>;
	var ret : Null<ComplexType>;
	var expr : Null<Expr>;
	var params : Array<TypeParamDecl>;
}

typedef FunctionArg = {
	var name : String;
	var opt : Bool;
	var type : Null<ComplexType>;
	@:optional var value : Null<Expr>;
}

typedef Metadata = Array<{ name : String, params : Array<Expr>, pos : Position }>;

typedef Field = {
	var name : String;
	@:optional var doc : Null<String>;
	@:optional var access : Array<Access>;
	var kind : FieldType;
	var pos : Position;
	@:optional var meta : Metadata;
}

enum Access {
	APublic;
	APrivate;
	AStatic;
	AOverride;
	ADynamic;
	AInline;
}

enum FieldType {
	FVar( t : Null<ComplexType>, ?e : Null<Expr> );
	FFun( f : Function );
	FProp( get : String, set : String, t : ComplexType, ?e : Null<Expr> );
}

typedef TypeDefinition = {
	var pack : Array<String>;
	var name : String;
	var pos : Position;
	var meta : Metadata;
	var params : Array<TypeParamDecl>;
	var isExtern : Bool;
	var kind : TypeDefKind;
	var fields : Array<Field>;
}

enum TypeDefKind {
	TDEnum;
	TDStructure;
	TDClass( ?extend : TypePath, ?implement : Array<TypePath>, ?isInterface : Bool );
}

/**
	This error can be used to handle or produce compilation errors in macros.
**/
class Error {
	public var message : String;
	public var pos : Expr.Position;
	public function new(m,p) {
		this.message = m;
		this.pos = p;
	}
}
