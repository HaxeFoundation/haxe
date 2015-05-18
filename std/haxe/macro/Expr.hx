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

#if (macro && !doc_gen)
extern enum Position {
}
#else
/**
	Represents a position in a file.
**/
typedef Position = {
	/**
		Reference to the filename.
	**/
	var file : String;
	
	/**
		Position of the first character.
	**/
	var min : Int;
	
	/**
		Position of the last character.
	**/
	var max : Int;
}
#end

/**
	Represents a constant.
	@see http://haxe.org/manual/expression-constants.html
**/
enum Constant {
	/**
		Represents a integer literal.
	**/
	CInt( v : String );
	
	/*
		Represents a float literal.
	**/
	CFloat( f : String );
	
	/*
		Represents a string literal.
	**/
	CString( s : String );
	
	/*
		Represents a indentifier literal.
	**/
	CIdent( s : String );
	
	/*
		Represents a regular expression literal.
	**/
	CRegexp( r : String, opt : String );
}

/**
	A binary operator.
	@see http://haxe.org/manual/types-numeric-operators.html
**/
enum Binop {
	/**
		`+`
	**/
	OpAdd;
	/**
		`*`
	**/
	OpMult;
	/**
		`/`
	**/
	OpDiv;
	/**
		`-`
	**/
	OpSub;
	/**
		`=`
	**/
	OpAssign;
	/**
		`==`
	**/
	OpEq;
	/**
		`!=`
	**/
	OpNotEq;
	/**
		`>`
	**/
	OpGt;
	/**
		`>=`
	**/
	OpGte;
	/**
		`<`
	**/
	OpLt;
	/**
		`<=`
	**/
	OpLte;
	/**
		`&`
	**/
	OpAnd;
	/**
		`|`
	**/
	OpOr;
	/**
		`^`
	**/
	OpXor;
	/**
		`&&`
	**/
	OpBoolAnd;
	/**
		`||`
	**/
	OpBoolOr;
	/**
		`<<`
	**/
	OpShl;
	/**
		`>>`
	**/
	OpShr;
	/**
		`>>>`
	**/
	OpUShr;
	/**
		`%`
	**/
	OpMod;
	/**
		`+=`
		`-=`
		`/=`
		`*=`
		`<<=`
		`>>=`
		`>>>=`
		`|=`
		`&=`
		`^=`
		`%=`
	**/
	OpAssignOp( op : Binop );
	/**
		`...`
	**/
	OpInterval;
	/**
		`=>`
	**/
	OpArrow;
}

/**
	A unary operator.
	@see http://haxe.org/manual/types-numeric-operators.html
**/
enum Unop {
	/**
		`++`
	**/
	OpIncrement;
	/**
		`--`
	**/
	OpDecrement;
	/**
		`!`
	**/
	OpNot;
	/**
		`-`
	**/
	OpNeg;
	/**
		`~`
	**/
	OpNegBits;
}

/**
	Represents a AST node for the AST.
	@see http://haxe.org/manual/macro-reification-expression.html
**/
typedef Expr = {
	/**
		The expression kind.
	**/
	var expr : ExprDef;
	
	/**
		The position of the expression.
	**/
	var pos : Position;
}

/**
	Represents a AST node identical to `Expr`, but it allows constraining the 
	type of accepted expressions.
	@see http://haxe.org/manual/macro-ExprOf.html
**/
typedef ExprOf<T> = Expr;

/**
	Represents a case for the AST.
	@see http://haxe.org/manual/expression-switch.html
**/
typedef Case = {
	/**
		The value expressions of the case.
	**/
	var values : Array<Expr>;
	/**
		The guard expressions of the case. Guards restrict cases.
	**/
	@:optional var guard : Null<Expr>;
	/**
		The expression of the case.
	**/
	var expr: Null<Expr>;
}

/**
	Represents a variable for the AST.
	@see http://haxe.org/manual/expression-var.html
**/
typedef Var = {
	/**
		The name of the variable.
	**/
	name : String,
	/**
		The type of the variable.
	**/
	type : Null<ComplexType>,
	/**
		The expression of the variable.
	**/
	expr : Null<Expr>
}

/**
	Represents a catch for the AST.
	@http://haxe.org/manual/expression-try-catch.html
**/
typedef Catch = {
	/**
		The name of the catch.
	**/
	name : String,
	/**
		The type of the catch.
	**/
	type : ComplexType,
	/**
		The expression of the catch.
	**/
	expr : Expr
}

/**
	Represents kind of a node for the AST.
**/
enum ExprDef {
	/**
		A constant.
	**/
	EConst( c : Constant );

	/**
		Array access `e1[e2]`.
	**/
	EArray( e1 : Expr, e2 : Expr );

	/**
		Binary operator `e1 op e2`.
	**/
	EBinop( op : Binop, e1 : Expr, e2 : Expr );

	/**
		Field access on `e` according to `field`.
	**/
	EField( e : Expr, field : String );

	/**
		Parentheses `(e)`.
	**/
	EParenthesis( e : Expr );

	/**
		An object declaration.
	**/
	EObjectDecl( fields : Array<{ field : String, expr : Expr }> );

	/**
		An array declaration `[el]`.
	**/
	EArrayDecl( values : Array<Expr> );

	/**
		A call `e(params)`.
	**/
	ECall( e : Expr, params : Array<Expr> );

	/**
		A constructor call `new t(params)`.
	**/
	ENew( t : TypePath, params : Array<Expr> );

	/**
		An unary operator `op` on `e`:
		
		* e++ (op = OpIncrement, postFix = true)
		* e-- (op = OpDecrement, postFix = true)
		* ++e (op = OpIncrement, postFix = false)
		* --e (op = OpDecrement, postFix = false)
		* -e (op = OpNeg, postFix = false)
		* !e (op = OpNot, postFix = false)
		* ~e (op = OpNegBits, postFix = false)
	**/
	EUnop( op : Unop, postFix : Bool, e : Expr );

	/**
		A variable declaration `var v` or `var v = expr`.
	**/
	EVars( vars : Array<Var> );

	/**
		A function declaration.
	**/
	EFunction( name : Null<String>, f : Function );

	/**
		A block declaration `{exprs}`.
	**/
	EBlock( exprs : Array<Expr> );

	/**
		A `for` expression.
	**/
	EFor( it : Expr, expr : Expr );

	/**
		A `(e1 in e2)` expression.
	**/
	EIn( e1 : Expr, e2 : Expr );

	/**
		An `if(econd) eif` or `if(econd) eif else eelse` expression.
	**/
	EIf( econd : Expr, eif : Expr, eelse : Null<Expr> );

	/**
		Represents a `while` expression.
		When `normalWhile` is `true` it is `while (...)`.
		When `normalWhile` is `false` it is `do {...} while (...)`.
	**/
	EWhile( econd : Expr, e : Expr, normalWhile : Bool );

	/**
		Represents a `switch` expression with related cases and an optional.
		`default` case if edef != null.
	**/
	ESwitch( e : Expr, cases : Array<Case>, edef : Null<Null<Expr>> );

	/**
		Represents a `try`-expression with related catches.
	**/
	ETry( e : Expr, catches : Array<Catch> );

	/**
		A `return` or `return e` expression.
	**/
	EReturn( ?e : Null<Expr> );

	/**
		A `break` expression.
	**/
	EBreak;

	/**
		A `continue` expression.
	**/
	EContinue;

	/**
		A `untyped e` expression.
	**/
	EUntyped( e : Expr );

	/**
		A `throw e` expression.
	**/
	EThrow( e : Expr );

	/**
		A `cast e` or `cast (e, m)` expression.
	**/
	ECast( e : Expr, t : Null<ComplexType> );

	/**
		Internally used to provide completion.
	**/
	EDisplay( e : Expr, isCall : Bool );

	/**
		Internally used to provide completion.
	**/
	EDisplayNew( t : TypePath );

	/**
		A `(econd) ? eif : eelse` expression.
	**/
	ETernary( econd : Expr, eif : Expr, eelse : Expr );

	/**
		A `(e:t)` expression.
	**/
	ECheckType( e : Expr, t : ComplexType );

	/**
		A `@m e` expression.
	**/
	EMeta( s : MetadataEntry, e : Expr );
}

/**
	Represents a complex type for the AST.
**/
enum ComplexType {
	/**
		Represents the type path.
	**/
	TPath( p : TypePath );

	/**
		Represents a function type.
		@see http://haxe.org/manual/types-function.html
	**/
	TFunction( args : Array<ComplexType>, ret : ComplexType );

	/**
		Represents an anonymous structure type.
		@see http://haxe.org/manual/types-anonymous-structure.html
	**/
	TAnonymous( fields : Array<Field> );

	/**
		Represents the parent type.
	**/
	TParent( t : ComplexType );

	/**
		Represents the structure to the type inheritance structure.
	**/
	TExtend( p : Array<TypePath>, fields : Array<Field> );

	/**
		Represents the optional type.
	**/
	TOptional( t : ComplexType );
}

/**
	Represents a type path for the AST.
**/
typedef TypePath = {
	/**
		Represents the package of the type path. 
	**/
	var pack : Array<String>;

	/**
		The name of the type path.
	**/
	var name : String;

	/**
		Optional parameters of the type path.
	**/
	@:optional var params : Array<TypeParam>;

	/**
		
	**/
	@:optional var sub : Null<String>;
}

/**
	Represents a type parameter for the AST.
**/
enum TypeParam {
	/**
		The type of the parameter.
	**/
	TPType( t : ComplexType );

	/**
		The expression node of the parameter.
	**/
	TPExpr( e : Expr );
}

/**
	Represents a type parameter declaration for the AST.
**/
typedef TypeParamDecl = {
	/**
		The name of the type parameter declaration.
	**/
	var name : String;

	/**
		The constraints of the type parameter declaration.
	**/
	@:optional var constraints : Array<ComplexType>;

	/**
		The parameters of the type parameter declaration.
	**/
	@:optional var params : Array<TypeParamDecl>;
}

/**
	Represents a function for the AST.
**/
typedef Function = {
	/**
		A list of function arguments.
	**/
	var args : Array<FunctionArg>;

	/**
		The return type of the function.
	**/
	var ret : Null<ComplexType>;

	/**
		The expression of the function body.
	**/
	var expr : Null<Expr>;

	/**
		An optional list of function parameter type declarations.
	**/
	@:optional var params : Array<TypeParamDecl>;
}

/**
	Represents a function argument for the AST.
**/
typedef FunctionArg = {
	/**
		The name of the function argument.
	**/
	var name : String;

	/**
		Whether or not the function argument is optional.
	**/
	@:optional var opt : Bool;

	/**
		The type of the function argument.
	**/
	var type : Null<ComplexType>;

	/**
		The optional value of the function argument.
	**/
	@:optional var value : Null<Expr>;
}

/**
	Represents a metadata entry for the AST.
**/
typedef MetadataEntry = {
	/**
		The name of the metadata entry.
	**/
	name : String,

	/**
		The optional parameters of the metadata entry.
	**/
	?params : Array<Expr>,

	/**
		The position of the metadata entry.
	**/
	pos : Position
}

/**
	Represents metadata for the AST.
**/
typedef Metadata = Array<MetadataEntry>;

/**
	Represents a field for the AST.
**/
typedef Field = {
	/**
		The name of the field.
	**/
	var name : String;

	/**
		The documentation of the field. If the field has no documentation, the 
		value is `null`.
	**/
	@:optional var doc : Null<String>;

	/**
		The access modifiers of the field. By default fields have private access.
		@see http://haxe.org/manual/class-field-access-modifier.html
	**/
	@:optional var access : Array<Access>;

	/**
		The type of the field.
	**/
	var kind : FieldType;

	/**
		The position of the field.
	**/
	var pos : Position;

	/**
		The optional metadata of the field.
	**/
	@:optional var meta : Metadata;
}

/**
	Represents an access modifier.
	@see http://haxe.org/manual/class-field-access-modifier.html
**/
enum Access {

	/**
		Public access modifier, grants access from anywhere.
		@see http://haxe.org/manual/class-field-visibility.html
	**/
	APublic;

	/**
		Private access modifier, grants access to class and its sub-classes 
		only.
		@see http://haxe.org/manual/class-field-visibility.html
	**/
	APrivate;

	/**
		Static access modifier. 
	**/
	AStatic;

	/**
		Override access modifier, required when a field is declared which also 
		exists on a parent class. This modifier is only allowed on 
		method fields.
	**/
	AOverride;

	/**
		Dynamic (re-)bindable access modifier. 
		@see http://haxe.org/manual/class-field-dynamic.html
	**/
	ADynamic;

	/**
		Inline access modifier. Allows expressions to be directly inserted in 
		place of calls to them.
	**/
	AInline;

	/**
		Macros access modifier. Allows expression macro functions. These are 
		normal functions which are executed as soon as they are typed.
	**/
	AMacro;
}

/**
	Represents the field type for the AST.
**/
enum FieldType {
	/**
		Represents a variable structure.
	**/
	FVar( t : Null<ComplexType>, ?e : Null<Expr> );

	/**
		Represents a function structure.
	**/
	FFun( f : Function );

	/**
		Represents a property with getter and setter structure.
	**/
	FProp( get : String, set : String, ?t : Null<ComplexType>, ?e : Null<Expr> );
}

/**
	Represents a type definition for the AST.
**/
typedef TypeDefinition = {
	/**
		The package of the type definition.
	**/
	var pack : Array<String>;

	/**
		The name of the type definition.
	**/
	var name : String;

	/**
		The position to the type definition.
	**/
	var pos : Position;

	/**
		The metadata of the type definition.
	**/
	@:optional var meta : Metadata;

	/**
		The paramater type declarations of the type definition.
	**/
	@:optional var params : Array<TypeParamDecl>;

	/**
		Whether or not the type is extern.
	**/
	@:optional var isExtern : Bool;

	/**
		The type kind of the type definition.
	**/
	var kind : TypeDefKind;

	/**
		The fields of the type definition.
	**/
	var fields : Array<Field>;
}

/**
	Represents a type definition kind for the AST.
**/
enum TypeDefKind {
	/**
		Represents an enum kind.
	**/
	TDEnum;

	/**
		Represents a structure kind.
	**/
	TDStructure;

	/**
		Represents a class kind.
	**/
	TDClass( ?superClass : TypePath, ?interfaces : Array<TypePath>, ?isInterface : Bool );

	/**
		Represents an alias kind.
	**/
	TDAlias( t : ComplexType ); // ignore TypeDefinition.fields

	/**
		Represents an abstract kind.
	**/
	TDAbstract( tthis : Null<ComplexType>, ?from : Array<ComplexType>, ?to: Array<ComplexType> );
}

/**
	This error can be used to handle or produce compilation errors in macros.
**/
class Error {

	/**
		The error message.
	**/
	public var message : String;

	/**
		The position of the error.
	**/
	public var pos : Expr.Position;

	/**
		Instantiates an error with given message and position.
	**/
	public function new(m,p) {
		this.message = m;
		this.pos = p;
	}

	/**
		String representation of the error.
	**/
	function toString() {
		return message;
	}
}

/**
	Represents the import mode.
	@see http://haxe.org/manual/type-system-import.html
**/
enum ImportMode {
	/**
		Represents a default import `import c`.
	**/
	INormal;

	/**
		Represents the alias import `import c in alias`.
	**/
	IAsName(alias:String);

	/**
		Represents the wildcard import `import *`.
	**/
	IAll;
}

/**
	Represents the import expression.
**/
typedef ImportExpr = {
	/**
		The path to the import expression.
	**/
	var path: Array< { pos: Position, name: String } >;

	/**
		The mode of the import expression.
	**/
	var mode: ImportMode;
}
