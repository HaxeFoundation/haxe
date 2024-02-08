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

package haxe.macro;

#if (macro && !doc_gen)
@:coreType abstract Position {}
#else

/**
	Represents a position in a file.
**/
typedef Position = {
	/**
		Reference to the filename.
	**/
	var file:String;

	/**
		Position of the first character.
	**/
	var min:Int;

	/**
		Position of the last character.
	**/
	var max:Int;
}
#end

enum StringLiteralKind {
	DoubleQuotes;
	SingleQuotes;
}

/**
	Represents a constant.
	@see https://haxe.org/manual/expression-constants.html
**/
enum Constant {
	/**
		Represents an integer literal.
	**/
	CInt(v:String, ?s:String);

	/**
		Represents a float literal.
	**/
	CFloat(f:String, ?s:String);

	/**
		Represents a string literal.
	**/
	CString(s:String, ?kind:StringLiteralKind);

	/**
		Represents an identifier.
	**/
	CIdent(s:String);

	/**
		Represents a regular expression literal.

		Example: `~/haxe/i`

		- The first argument `haxe` is a string with regular expression pattern.
		- The second argument `i` is a string with regular expression flags.

		@see https://haxe.org/manual/std-regex.html
	**/
	CRegexp(r:String, opt:String);
}

/**
	A binary operator.
	@see https://haxe.org/manual/types-numeric-operators.html
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
		`+=` `-=` `/=` `*=` `<<=` `>>=` `>>>=` `|=` `&=` `^=` `%=`
	**/
	OpAssignOp(op:Binop);

	/**
		`...`
	**/
	OpInterval;

	/**
		`=>`
	**/
	OpArrow;

	/**
		`in`
	**/
	OpIn;

	/**
		`??`
	**/
	OpNullCoal;
}

/**
	A unary operator.
	@see https://haxe.org/manual/types-numeric-operators.html
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

	/**
		`...`
	**/
	OpSpread;
}

enum EFieldKind {
	Normal;
	Safe;
}

/**
	Represents a node in the AST.
	@see https://haxe.org/manual/macro-reification-expression.html
**/
typedef Expr = {
	/**
		The expression kind.
	**/
	var expr:ExprDef;

	/**
		The position of the expression.
	**/
	var pos:Position;
}

/**
	Represents a AST node identical to `Expr`, but it allows constraining the
	type of accepted expressions.
	@see https://haxe.org/manual/macro-ExprOf.html
**/
typedef ExprOf<T> = Expr;

/**
	Represents a switch case.
	@see https://haxe.org/manual/expression-switch.html
**/
typedef Case = {
	/**
		The value expressions of the case.
	**/
	var values:Array<Expr>;

	/**
		The optional guard expressions of the case, if available.
	**/
	var ?guard:Expr;

	/**
		The expression of the case, if available.
	**/
	var ?expr:Expr;
}

/**
	Represents a variable in the AST.
	@see https://haxe.org/manual/expression-var.html
**/
typedef Var = {
	/**
		The name of the variable.
	**/
	var name:String;

	/**
		The position of the variable name.
	**/
	var ?namePos:Position;

	/**
		The type-hint of the variable, if available.
	**/
	var ?type:ComplexType;

	/**
		The expression of the variable, if available.
	**/
	var ?expr:Expr;

	/**
		Whether or not the variable can be assigned to.
	**/
	var ?isFinal:Bool;

	/**
		Whether or not the variable is static.
	**/
	var ?isStatic:Bool;

	/**
		Metadata associatied with the variable, if available.
	**/
	var ?meta:Metadata;
}

/**
	Represents a catch in the AST.
	@see https://haxe.org/manual/expression-try-catch.html
**/
typedef Catch = {
	/**
		The name of the catch variable.
	**/
	var name:String;

	/**
		The type of the catch.
	**/
	var ?type:ComplexType;

	/**
		The expression of the catch.
	**/
	var expr:Expr;
}

/**
	Represents the way something is quoted.
**/
enum QuoteStatus {
	/**
		No quotes
	**/
	Unquoted;

	/**
		Double quotes `"`
	**/
	Quoted;
}

/**
	Represents the field of an object declaration.
**/
typedef ObjectField = {
	/**
		The name of the field.
	**/
	var field:String;

	/**
		The field expression.
	**/
	var expr:Expr;

	/**
		How the field name is quoted.
	**/
	var ?quotes:QuoteStatus;
}

/**
	Represents function kind in the AST
**/
enum FunctionKind {
	/**
		Anonymous function
	**/
	FAnonymous;

	/**
		Named function
	**/
	FNamed(name:String, ?inlined:Bool);

	/**
		Arrow function
	**/
	FArrow;
}

/**
	Represents the kind of a node in the AST.
**/
enum ExprDef {
	/**
		A constant.
	**/
	EConst(c:Constant);

	/**
		Array access `e1[e2]`.
	**/
	EArray(e1:Expr, e2:Expr);

	/**
		Binary operator `e1 op e2`.
	**/
	EBinop(op:Binop, e1:Expr, e2:Expr);

	/**
		Field access on `e.field`.

		If `kind` is null, it is equal to Normal.
	**/
	EField(e:Expr, field:String, ?kind:EFieldKind);

	/**
		Parentheses `(e)`.
	**/
	EParenthesis(e:Expr);

	/**
		An object declaration.
	**/
	EObjectDecl(fields:Array<ObjectField>);

	/**
		An array declaration `[el]`.
	**/
	EArrayDecl(values:Array<Expr>);

	/**
		A call `e(params)`.
	**/
	ECall(e:Expr, params:Array<Expr>);

	/**
		A constructor call `new t(params)`.
	**/
	ENew(t:TypePath, params:Array<Expr>);

	/**
		An unary operator `op` on `e`:

		- `e++` (`op = OpIncrement, postFix = true`)
		- `e--` (`op = OpDecrement, postFix = true`)
		- `++e` (`op = OpIncrement, postFix = false`)
		- `--e` (`op = OpDecrement, postFix = false`)
		- `-e` (`op = OpNeg, postFix = false`)
		- `!e` (`op = OpNot, postFix = false`)
		- `~e` (`op = OpNegBits, postFix = false`)
	**/
	EUnop(op:Unop, postFix:Bool, e:Expr);

	/**
		Variable declarations.
	**/
	EVars(vars:Array<Var>);

	/**
		A function declaration.
	**/
	EFunction(kind:Null<FunctionKind>, f:Function);

	/**
		A block of expressions `{exprs}`.
	**/
	EBlock(exprs:Array<Expr>);

	/**
		A `for` expression.
	**/
	EFor(it:Expr, expr:Expr);

	/**
		An `if (econd) eif` or `if (econd) eif else eelse` expression.
	**/
	EIf(econd:Expr, eif:Expr, eelse:Null<Expr>);

	/**
		Represents a `while` expression.

		When `normalWhile` is `true` it is `while (...)`.

		When `normalWhile` is `false` it is `do {...} while (...)`.
	**/
	EWhile(econd:Expr, e:Expr, normalWhile:Bool);

	/**
		Represents a `switch` expression with related cases and an optional.
		`default` case if `edef != null`.
	**/
	ESwitch(e:Expr, cases:Array<Case>, edef:Null<Expr>);

	/**
		Represents a `try`-expression with related catches.
	**/
	ETry(e:Expr, catches:Array<Catch>);

	/**
		A `return` or `return e` expression.
	**/
	EReturn(?e:Expr);

	/**
		A `break` expression.
	**/
	EBreak;

	/**
		A `continue` expression.
	**/
	EContinue;

	/**
		An `untyped e` source code.
	**/
	EUntyped(e:Expr);

	/**
		A `throw e` expression.
	**/
	EThrow(e:Expr);

	/**
		A `cast e` or `cast (e, m)` expression.
	**/
	ECast(e:Expr, t:Null<ComplexType>);

	/**
		Used internally to provide completion.
	**/
	EDisplay(e:Expr, displayKind:DisplayKind);

	/**
		A `(econd) ? eif : eelse` expression.
	**/
	ETernary(econd:Expr, eif:Expr, eelse:Expr);

	/**
		A `(e:t)` expression.
	**/
	ECheckType(e:Expr, t:ComplexType);

	/**
		A `@m e` expression.
	**/
	EMeta(s:MetadataEntry, e:Expr);

	/**
		An `expr is Type` expression.
	**/
	EIs(e:Expr, t:ComplexType);
}

enum DisplayKind {
	DKCall;
	DKDot;
	DKStructure;
	DKMarked;
	DKPattern(outermost:Bool);
}

/**
	Represents a type syntax in the AST.
**/
enum ComplexType {
	/**
		Represents the type path.
	**/
	TPath(p:TypePath);

	/**
		Represents a function type.
		@see https://haxe.org/manual/types-function.html
	**/
	TFunction(args:Array<ComplexType>, ret:ComplexType);

	/**
		Represents an anonymous structure type.
		@see https://haxe.org/manual/types-anonymous-structure.html
	**/
	TAnonymous(fields:Array<Field>);

	/**
		Represents parentheses around a type, e.g. the `(Int -> Void)` part in
		`(Int -> Void) -> String`.
	**/
	TParent(t:ComplexType);

	/**
		Represents typedef extensions `> Iterable<T>`.
		The array `p` holds the type paths to the given types.
		@see https://haxe.org/manual/type-system-extensions.html
	**/
	TExtend(p:Array<TypePath>, fields:Array<Field>);

	/**
		Represents an optional type.
	**/
	TOptional(t:ComplexType);

	/**
		Represents a type with a name.
	**/
	TNamed(n:String, t:ComplexType);

	/**
		Represents an intersection type `T1 & T2 & ... & TN`.
	**/
	TIntersection(tl:Array<ComplexType>);
}

/**
	Represents a type path in the AST.
**/
typedef TypePath = {
	/**
		Represents the package of the type path.
	**/
	var pack:Array<String>;

	/**
		The name of the type path.
	**/
	var name:String;

	/**
		Optional parameters of the type path.
	**/
	var ?params:Array<TypeParam>;

	/**
		Sub is set on module sub-type access:
		`pack.Module.Type` has `name = "Module"`, `sub = "Type"`, if available.
	**/
	var ?sub:String;
}

/**
	Represents a concrete type parameter in the AST.

	Haxe allows expressions in concrete type parameters, e.g.
	`new YourType<["hello", "world"]>`. In that case the value is `TPExpr` while
	in the normal case it's `TPType`.
**/
enum TypeParam {
	TPType(t:ComplexType);
	TPExpr(e:Expr);
}

/**
	Represents a type parameter declaration in the AST.
**/
typedef TypeParamDecl = {
	/**
		The name of the type parameter.
	**/
	var name:String;

	/**
		The optional constraints of the type parameter.
	**/
	var ?constraints:Array<ComplexType>;

	/**
		The optional default type of the type parameter.
	**/
	var ?defaultType:Null<ComplexType>;

	/**
		The optional parameters of the type parameter.
	**/
	var ?params:Array<TypeParamDecl>;

	/**
		The metadata of the type parameter.
	**/
	var ?meta:Metadata;
}

/**
	Represents a function in the AST.
**/
typedef Function = {
	/**
		A list of function arguments.
	**/
	var args:Array<FunctionArg>;

	/**
		The return type-hint of the function, if available.
	**/
	var ?ret:ComplexType;

	/**
		The expression of the function body, if available.
	**/
	var ?expr:Expr;

	/**
		An optional list of function parameter type declarations.
	**/
	var ?params:Array<TypeParamDecl>;
}

/**
	Represents a function argument in the AST.
**/
typedef FunctionArg = {
	/**
		The name of the function argument.
	**/
	var name:String;

	/**
		Whether or not the function argument is optional.
	**/
	var ?opt:Bool;

	/**
		The type-hint of the function argument, if available.
	**/
	var ?type:ComplexType;

	/**
		The optional value of the function argument, if available.
	**/
	var ?value:Expr;

	/**
		The metadata of the function argument.
	**/
	var ?meta:Metadata;
}

/**
	Represents a metadata entry in the AST.
**/
typedef MetadataEntry = {
	/**
		The name of the metadata entry.
	**/
	var name:String;

	/**
		The optional parameters of the metadata entry.
	**/
	var ?params:Array<Expr>;

	/**
		The position of the metadata entry.
	**/
	var pos:Position;
}

/**
	Represents metadata in the AST.
**/
typedef Metadata = Array<MetadataEntry>;

/**
	Represents a field in the AST.
**/
typedef Field = {
	/**
		The name of the field.
	**/
	var name:String;

	/**
		The documentation of the field, if available. If the field has no
		documentation, the value is `null`.
	**/
	var ?doc:String;

	/**
		The access modifiers of the field. By default fields have private access.
		@see https://haxe.org/manual/class-field-access-modifier.html
	**/
	var ?access:Array<Access>;

	/**
		The kind of the field.
	**/
	var kind:FieldType;

	/**
		The position of the field.
	**/
	var pos:Position;

	/**
		The optional metadata of the field.
	**/
	var ?meta:Metadata;
}

/**
	Represents an access modifier.
	@see https://haxe.org/manual/class-field-access-modifier.html
**/
enum Access {
	/**
		Public access modifier, grants access from anywhere.
		@see https://haxe.org/manual/class-field-visibility.html
	**/
	APublic;

	/**
		Private access modifier, grants access to class and its sub-classes
		only.
		@see https://haxe.org/manual/class-field-visibility.html
	**/
	APrivate;

	/**
		Static access modifier.
	**/
	AStatic;

	/**
		Override access modifier.
		@see https://haxe.org/manual/class-field-override.html
	**/
	AOverride;

	/**
		Dynamic (re-)bindable access modifier.
		@see https://haxe.org/manual/class-field-dynamic.html
	**/
	ADynamic;

	/**
		Inline access modifier. Allows expressions to be directly inserted in
		place of calls to them.
		@see https://haxe.org/manual/class-field-inline.html
	**/
	AInline;

	/**
		Macro access modifier. Allows expression macro functions. These are
		normal functions which are executed as soon as they are typed.
	**/
	AMacro;

	/**
		Final access modifier. For functions, they can not be overridden. For
		variables, it means they can be assigned to only once.
	**/
	AFinal;

	/**
		Extern access modifier.
	**/
	AExtern;

	/**
		Abstract access modifier.
	**/
	AAbstract;

	/**
		Overload access modifier.
	**/
	AOverload;

	/**
		Enum access modifier.
	**/
	AEnum;
}

/**
	Represents the field type in the AST.
**/
enum FieldType {
	/**
		Represents a variable field type.
	**/
	FVar(t:Null<ComplexType>, ?e:Expr);

	/**
		Represents a function field type.
	**/
	FFun(f:Function);

	/**
		Represents a property with getter and setter field type.
	**/
	FProp(get:String, set:String, ?t:ComplexType, ?e:Expr);
}

/**
	Represents a type definition.
**/
typedef TypeDefinition = {
	/**
		The package of the type definition.
	**/
	var pack:Array<String>;

	/**
		The name of the type definition.
	**/
	var name:String;

	/**
		The documentation of the type, if available. If the type has no
		documentation, the value is `null`.
	**/
	var ?doc:String;

	/**
		The position to the type definition.
	**/
	var pos:Position;

	/**
		The optional metadata of the type definition.
	**/
	var ?meta:Metadata;

	/**
		The parameter type declarations of the type definition.
	**/
	var ?params:Array<TypeParamDecl>;

	/**
		Whether or not the type is extern.
	**/
	var ?isExtern:Bool;

	/**
		The kind of the type definition.
	**/
	var kind:TypeDefKind;

	/**
		The fields of the type definition.
	**/
	var fields:Array<Field>;
}

/**
	Represents a type definition kind.
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
	TDClass(?superClass:TypePath, ?interfaces:Array<TypePath>, ?isInterface:Bool, ?isFinal:Bool, ?isAbstract:Bool);

	/**
		Represents an alias/typedef kind.
	**/
	TDAlias(t:ComplexType); // ignore TypeDefinition.fields

	/**
		Represents an abstract kind.
	**/
	TDAbstract(tthis:Null<ComplexType>, ?flags:Array<AbstractFlag>, ?from:Array<ComplexType>, ?to:Array<ComplexType>);

	/**
		Represents a module-level field.
	**/
	TDField(kind:FieldType, ?access:Array<Access>); // ignore TypeDefinition.fields

}

/**
	Represents an abstract flag.
**/
enum AbstractFlag {
	/**
		Indicates that this abstract is an `enum abstract`
	**/
	AbEnum;

	/**
		Indicates that this abstract can be assigned from `ct`.
		This flag can be added several times to add multiple "from" types.
	**/
	AbFrom(ct:ComplexType);

	/**
		Indicates that this abstract can be assigned to `ct`.
		This flag can be added several times to add multiple "to" types.
	**/
	AbTo(ct:ComplexType);
}

/**
	This error can be used to handle or produce compilation errors in macros.
**/
class Error extends Exception {
	/**
		The position of the error.
	**/
	public var pos:Position;

	/**
		Child error messages, if any.
	**/
	private var childErrors:Array<Error>;

	/**
		Instantiates an error with given message and position.
	**/
	public function new(message:String, pos:Position, ?previous:Exception) {
		super(message, previous);
		this.pos = pos;
	}
}

/**
	Represents the import mode.
	@see https://haxe.org/manual/type-system-import.html
**/
enum ImportMode {
	/**
		Represents a default import `import c`.
	**/
	INormal;

	/**
		Represents the alias import `import c as alias`.
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
	var path:Array<{pos:Position, name:String}>;

	/**
		The mode of the import expression.
	**/
	var mode:ImportMode;
}
