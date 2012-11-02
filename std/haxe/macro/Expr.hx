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
	EMeta( s : MetadataEntry, e : Expr );
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

typedef MetadataEntry = {
	name : String,
	params : Array<Expr>,
	pos : Position
}

typedef Metadata = Array<MetadataEntry>;

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
	FProp( get : String, set : String, ?t : Null<ComplexType>, ?e : Null<Expr> );
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
	TDAlias( t : ComplexType ); // ignore TypeDefinition.fields
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
