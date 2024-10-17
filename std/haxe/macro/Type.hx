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

/*
	Warning: Some of these types correspond to compiler-internal data structures
	and might change in minor Haxe releases in order to adapt to internal changes.
 */
/**
	Represents a reference to internal compiler structure. It exists to avoid
	expensive encoding if it is not required and to ensure that physical
	equality remains intact.

	A structure is only encoded when user requests it through `ref.get()`.
 */
typedef Ref<T> = {
	public function get():T;
	public function toString():String;
}

/**
	Represents a type.
 */
enum Type {
	/**
		Represents a monomorph.

		@see https://haxe.org/manual/types-monomorph.html
	**/
	TMono(t:Ref<Null<Type>>);

	/**
		Represents an enum instance.

		@see https://haxe.org/manual/types-enum-instance.html
	**/
	TEnum(t:Ref<EnumType>, params:Array<Type>);

	/**
		Represents a class instance.

		@see https://haxe.org/manual/types-class-instance.html
	**/
	TInst(t:Ref<ClassType>, params:Array<Type>);

	/**
		Represents a typedef.

		@see https://haxe.org/manual/type-system-typedef.html
	**/
	TType(t:Ref<DefType>, params:Array<Type>);

	/**
		Represents a function type.

		@see https://haxe.org/manual/types-function.html
	**/
	TFun(args:Array<{name:String, opt:Bool, t:Type}>, ret:Type);

	/**
		Represents an anonymous structure type.

		@see https://haxe.org/manual/types-anonymous-structure.html
	**/
	TAnonymous(a:Ref<AnonType>);

	/**
		Represents Dynamic.

		@see https://haxe.org/manual/types-dynamic.html
	**/
	TDynamic(t:Null<Type>);

	/**
		Used internally by the compiler to delay some typing.
	**/
	TLazy(f:Void->Type);

	/**
		Represents an abstract type.

		@see https://haxe.org/manual/types-abstract.html
	**/
	TAbstract(t:Ref<AbstractType>, params:Array<Type>);
}

/**
	Represents information for anonymous structure types.
 */
typedef AnonType = {
	/**
		The class fields of the structure.
	**/
	var fields:Array<ClassField>;

	/**
		The status/kind of the structure.
	**/
	var status:AnonStatus;
}

/**
	Represents the kind of the anonymous structure type.
**/
enum AnonStatus {
	/**
		A closed structure is considered complete. That is, no further fields
		can be added to it.
	**/
	AClosed;

	/**
		An open structure allows having additional fields added to it, which is
		used during type inference. It is closed upon unification.
	**/
	AOpened;

	/**
		A const structure is one that appears directly in syntax. It cannot be
		assigned to a smaller structure type (that is, it does not allow
		structural sub-typing).
	**/
	AConst;

	/**
		Represents a structure which extends one or multiple structures defined
		in `tl`.

		@see https://haxe.org/manual/type-system-extensions.html
	**/
	AExtend(tl:Ref<Array<Type>>);

	/**
		A structure that represents the static fields of a class.
	**/
	AClassStatics(t:Ref<ClassType>);

	/**
		A structure that represents the constructors of an enum.
	**/
	AEnumStatics(t:Ref<EnumType>);

	/**
		A structure that represents the static fields of an abstract.
	**/
	AAbstractStatics(t:Ref<AbstractType>);
}

/**
	Represents the declaration of type parameters.
 */
typedef TypeParameter = {
	/**
		The name of the type parameter.
	**/
	var name:String;

	/**
		The type of the type parameter. It is guaranteed to be a `TInst` with a
		`KTypeParameter` kind.
	**/
	var t:Type;

	/**
		The default type for this type parameter.
	**/
	var ?defaultType:Null<Type>;
}

/**
	Represents a class field.
 */
typedef ClassField = {
	/**
		The name of the class field.
	**/
	var name:String;

	/**
		The type of the class field.
	**/
	var type:Type;

	/**
		Whether or not the class field is public.
	**/
	var isPublic:Bool;

	/**
		Whether or not the class field is extern.
	**/
	var isExtern:Bool;

	/**
		Whether or not the class field is final.
	**/
	var isFinal:Bool;

	/**
		Whether or not the class field is abstract.
	**/
	var isAbstract:Bool;

	/**
		The type parameters of the class field.
	**/
	var params:Array<TypeParameter>;

	/**
		The metadata of the class field.
	**/
	var meta:MetaAccess;

	/**
		The class field kind.
	**/
	var kind:FieldKind;

	/**
		Returns the typed expression of the class field.
	**/
	function expr():Null<TypedExpr>;

	/**
		The position of the class field.
	**/
	var pos:Expr.Position;

	/**
		The associated documentation of the class field.
	**/
	var doc:Null<String>;

	/**
		The overload fields of the class field.
	**/
	var overloads:Ref<Array<ClassField>>;
}

/**
	Represents an enum constructor.
 */
typedef EnumField = {
	/**
		The name of the enum constructor.
	**/
	var name:String;

	/**
		The type of the enum constructor.
	**/
	var type:Type;

	/**
		The position of the enum constructor.
	**/
	var pos:Expr.Position;

	/**
		The metadata of the enum constructor.
	**/
	var meta:MetaAccess;

	/**
		The index of the enum constructor, i.e. in which position it appears
		in the syntax.
	**/
	var index:Int;

	/**
		The associated documentation of the enum constructor.
	**/
	var doc:Null<String>;

	/**
		The type parameters of the enum constructor.
	**/
	var params:Array<TypeParameter>;
}

/**
	Represents the kind of a class.
 */
enum ClassKind {
	/**
		A normal class.
	**/
	KNormal;

	/**
		A type parameter class with a set of constraints.
	**/
	KTypeParameter(constraints:Array<Type>);

	/**
		A class containing module fields.
	**/
	KModuleFields(module:String);

	/**
		A special kind of class to encode expressions into type parameters.
	**/
	KExpr(expr:Expr);

	/**
		A `@:generic` base class.
	**/
	KGeneric;

	/**
		A concrete `@:generic` instance, referencing the original class and the
		applied type parameters.
	**/
	KGenericInstance(cl:Ref<ClassType>, params:Array<Type>);

	/**
		A special class for `haxe.macro.MacroType`.

		@deprecated
	**/
	KMacroType;

	/**
		An implementation class of an abstract, i.e. where all its run-time code
		is.
	**/
	KAbstractImpl(a:Ref<AbstractType>);

	/**
		A `@:genericBuild` class
	**/
	KGenericBuild;
}

/**
	The information that all types (`ClassType`, `EnumType`, `DefType`,
	`AbstractType`) have in common.
**/
typedef BaseType = {
	/**
		The package of the type.
	**/
	var pack:Array<String>;

	/**
		The name of the type.
	**/
	var name:String;

	/**
		The module name of the type, which might be different.
	**/
	var module:String;

	/**
		The position of the type.
	**/
	var pos:Expr.Position;

	/**
		Whether or not the type is private.
	**/
	var isPrivate:Bool;

	/**
		Whether or not the type is extern.
	**/
	var isExtern:Bool;

	/**
		The type parameters of the type.
	**/
	var params:Array<TypeParameter>;

	/**
		The metadata of the type.
	**/
	var meta:MetaAccess;

	/**
		The associated documentation of the class field.
	**/
	var doc:Null<String>;

	/**
		Allows excluding the type from compilation.
	**/
	function exclude():Void;
}

/**
	Represents a class type.
 */
typedef ClassType = BaseType & {
	/**
		The kind of the class.
	**/
	var kind:ClassKind;

	/**
		If true the type is an interface, otherwise it is a class.
	**/
	var isInterface:Bool;

	/**
		If true the class is final and cannot be extended.
	**/
	var isFinal:Bool;

	/**
		If true the class is abstract and cannot be instantiated directly.
	**/
	var isAbstract:Bool;

	/**
		The parent class and its type parameters, if available.
	**/
	var superClass:Null<{t:Ref<ClassType>, params:Array<Type>}>;

	/**
		The implemented interfaces and their type parameters.
	**/
	var interfaces:Array<{t:Ref<ClassType>, params:Array<Type>}>;

	/**
		The member fields of the class.
	**/
	var fields:Ref<Array<ClassField>>;

	/**
		The static fields of the class.
	**/
	var statics:Ref<Array<ClassField>>;

	// var dynamic : Null<Type>;
	// var arrayAccess : Null<Type>;

	/**
		The constructor of the class, if available.
	**/
	var constructor:Null<Ref<ClassField>>;

	/**
		The `__init__` expression of the class, if available.
	**/
	var init:Null<TypedExpr>;

	/**
		The list of fields that have override status.
	**/
	var overrides:Array<Ref<ClassField>>;
}

/**
	Represents an enum type.
 */
typedef EnumType = BaseType & {
	/**
		The available enum constructors.
	**/
	var constructs:Map<String, EnumField>;

	/**
		An ordered list of enum constructor names.
	**/
	var names:Array<String>;
}

/**
	Represents a typedef.
 */
typedef DefType = BaseType & {
	/**
		The target type of the typedef.
	**/
	var type:Type;
}

/**
	Represents an abstract type.
 */
typedef AbstractType = BaseType & {
	/**
		The underlying type of the abstract.
	**/
	var type:Type;

	/**
		The implementation class of the abstract, if available.
	**/
	var impl:Null<Ref<ClassType>>;

	/**
		The defined binary operators of the abstract.
	**/
	var binops:Array<{op:Expr.Binop, field:ClassField}>;

	/**
		The defined unary operators of the abstract.
	**/
	var unops:Array<{op:Expr.Unop, postFix:Bool, field:ClassField}>;

	/**
		The available implicit from-casts of the abstract.

		@see https://haxe.org/manual/types-abstract-implicit-casts.html
	**/
	var from:Array<{t:Type, field:Null<ClassField>}>;

	/**
		The available implicit to-casts of the abstract.

		@see https://haxe.org/manual/types-abstract-implicit-casts.html
	**/
	var to:Array<{t:Type, field:Null<ClassField>}>;

	/**
		The defined array-access fields of the abstract.
	**/
	var array:Array<ClassField>;

	/**
		The method used for resolving unknown field access, if available.
	**/
	var resolve:Null<ClassField>;

	/**
		The method used for resolving unknown field access, if available.
	**/
	var resolveWrite:Null<ClassField>;
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
	function get():Expr.Metadata;

	/**
		Extract metadata entries by given `name`.

		If there's no metadata with such name, empty array `[]` is returned.

		If `name` is null, compilation fails with an error.
	**/
	function extract(name:String):Array<Expr.MetadataEntry>;

	/**
		Adds the metadata specified by `name`, `params` and `pos` to the origin
		of `this` MetaAccess.

		Metadata names are not unique during compilation, so this method never
		overwrites a previous metadata.

		If a `Metadata` array is obtained through a call to `get`, a subsequent
		call to `add` has no effect on that array.

		If any argument is null, compilation fails with an error.
	**/
	function add(name:String, params:Array<Expr>, pos:Expr.Position):Void;

	/**
		Removes all `name` metadata entries from the origin of `this`
		MetaAccess.

		This method might clear several metadata entries of the same name.

		If a `Metadata` array is obtained through a call to `get`, a subsequent
		call to `remove` has no effect on that array.

		If `name` is null, compilation fails with an error.
	**/
	function remove(name:String):Void;

	/**
		Tells if the origin of `this` MetaAccess has a `name` metadata entry.

		If `name` is null, compilation fails with an error.
	**/
	function has(name:String):Bool;
}

/**
	Represents a field kind.
 */
enum FieldKind {
	/**
		A variable or property, depending on the `read` and `write` values.
	**/
	FVar(read:VarAccess, write:VarAccess);

	/**
		A method
	**/
	FMethod(k:MethodKind);
}

/**
	Represents the variable accessor.
 */
enum VarAccess {
	/**
		Normal access (`default`).
	**/
	AccNormal;

	/**
		Private access (`null`).
	**/
	AccNo;

	/**
		No access (`never`).
	**/
	AccNever;

	/**
		Unused.
	**/
	AccResolve;

	/**
		Access through accessor function (`get`, `set`, `dynamic`).
	**/
	AccCall;

	/**
		Inline access (`inline`).
	**/
	AccInline;

	/**
		Failed access due to a `@:require` metadata.
	**/
	AccRequire(r:String, ?msg:String);

	/**
		Access is only allowed from the constructor.
	**/
	AccCtor;
}

/**
	Represents the method kind.
 */
enum MethodKind {
	/**
		A normal method.
	**/
	MethNormal;

	/**
		An inline method.

		@see https://haxe.org/manual/class-field-inline.html
	**/
	MethInline;

	/**
		A dynamic, rebindable method.

		@see https://haxe.org/manual/class-field-dynamic.html
	**/
	MethDynamic;

	/**
		A macro method.
	**/
	MethMacro;
}

/**
	Represents typed constant.
 */
enum TConstant {
	/**
		An `Int` literal.
	**/
	TInt(i:Int);

	/**
		A `Float` literal, represented as String to avoid precision loss.
	**/
	TFloat(s:String);

	/**
		A `String` literal.
	**/
	TString(s:String);

	/**
		A `Bool` literal.
	**/
	TBool(b:Bool);

	/**
		The constant `null`.
	**/
	TNull;

	/**
		The constant `this`.
	**/
	TThis;

	/**
		The constant `super`.
	**/
	TSuper;
}

/**
	Represents a module type. These are the types that can be declared in a Haxe
	module and which are passed to the generators (except `TTypeDecl`).
 */
enum ModuleType {
	/**
		A class.
	**/
	TClassDecl(c:Ref<ClassType>);

	/**
		An enum.
	**/
	TEnumDecl(e:Ref<EnumType>);

	/**
		A typedef.
	**/
	TTypeDecl(t:Ref<DefType>);

	/**
		An abstract.
	**/
	TAbstract(a:Ref<AbstractType>);
}

/**
	Represents a function in the typed AST.
 */
typedef TFunc = {
	/**
		A list of function arguments identified by an argument variable `v` and
		an optional initialization `value`.
	**/
	var args:Array<{v:TVar, value:Null<TypedExpr>}>;

	/**
		The return type of the function.
	**/
	var t:Type;

	/**
		The expression of the function body.
	**/
	var expr:TypedExpr;
}

/**
	Represents the kind of field access in the typed AST.
 */
enum FieldAccess {
	/**
		Access of field `cf` on a class instance `c` with type parameters
		`params`.
	**/
	FInstance(c:Ref<ClassType>, params:Array<Type>, cf:Ref<ClassField>);

	/**
		Static access of a field `cf` on a class `c`.
	**/
	FStatic(c:Ref<ClassType>, cf:Ref<ClassField>);

	/**
		Access of field `cf` on an anonymous structure.
	**/
	FAnon(cf:Ref<ClassField>);

	/**
		Dynamic field access of a field named `s`.
	**/
	FDynamic(s:String);

	/**
		Closure field access of field `cf` on a class instance `c` with type
		parameters `params`.
	**/
	FClosure(c:Null<{c:Ref<ClassType>, params:Array<Type>}>, cf:Ref<ClassField>);

	/**
		Field access to an enum constructor `ef` of enum `e`.
	**/
	FEnum(e:Ref<EnumType>, ef:EnumField);
}

/**
	Represents kind of a node in the typed AST.
 */
enum TypedExprDef {
	/**
		A constant.
	**/
	TConst(c:TConstant);

	/**
		Reference to a local variable `v`.
	**/
	TLocal(v:TVar);

	/**
		Array access `e1[e2]`.
	**/
	TArray(e1:TypedExpr, e2:TypedExpr);

	/**
		Binary operator `e1 op e2`.
	**/
	TBinop(op:Expr.Binop, e1:TypedExpr, e2:TypedExpr);

	/**
		Field access on `e` according to `fa`.
	**/
	TField(e:TypedExpr, fa:FieldAccess);

	/**
		Reference to a module type `m`.
	**/
	TTypeExpr(m:ModuleType);

	/**
		Parentheses `(e)`.
	**/
	TParenthesis(e:TypedExpr);

	/**
		An object declaration.
	**/
	TObjectDecl(fields:Array<{name:String, expr:TypedExpr}>);

	/**
		An array declaration `[el]`.
	**/
	TArrayDecl(el:Array<TypedExpr>);

	/**
		A call `e(el)`.
	**/
	TCall(e:TypedExpr, el:Array<TypedExpr>);

	/**
		A constructor call `new c<params>(el)`.
	**/
	TNew(c:Ref<ClassType>, params:Array<Type>, el:Array<TypedExpr>);

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
	TUnop(op:Expr.Unop, postFix:Bool, e:TypedExpr);

	/**
		A function declaration.
	**/
	TFunction(tfunc:TFunc);

	/**
		A variable declaration `var v` or `var v = expr`.
	**/
	TVar(v:TVar, expr:Null<TypedExpr>);

	/**
		A block declaration `{el}`.
	**/
	TBlock(el:Array<TypedExpr>);

	/**
		A `for` expression.
	**/
	TFor(v:TVar, e1:TypedExpr, e2:TypedExpr);

	/**
		An `if(econd) eif` or `if(econd) eif else eelse` expression.
	**/
	TIf(econd:TypedExpr, eif:TypedExpr, eelse:Null<TypedExpr>);

	/**
		Represents a `while` expression.
		When `normalWhile` is `true` it is `while (...)`.
		When `normalWhile` is `false` it is `do {...} while (...)`.
	**/
	TWhile(econd:TypedExpr, e:TypedExpr, normalWhile:Bool);

	/**
		Represents a `switch` expression with related cases and an optional
		`default` case if edef != null.
	**/
	TSwitch(e:TypedExpr, cases:Array<{values:Array<TypedExpr>, expr:TypedExpr}>, edef:Null<TypedExpr>);

	/**
		Represents a `try`-expression with related catches.
	**/
	TTry(e:TypedExpr, catches:Array<{v:TVar, expr:TypedExpr}>);

	/**
		A `return` or `return e` expression.
	**/
	TReturn(e:Null<TypedExpr>);

	/**
		A `break` expression.
	**/
	TBreak;

	/**
		A `continue` expression.
	**/
	TContinue;

	/**
		A `throw e` expression.
	**/
	TThrow(e:TypedExpr);

	/**
		A `cast e` or `cast (e, m)` expression.
	**/
	TCast(e:TypedExpr, m:Null<ModuleType>);

	/**
		A `@m e1` expression.
	**/
	TMeta(m:Expr.MetadataEntry, e1:TypedExpr);

	/**
		Access to an enum parameter (generated by the pattern matcher).
	**/
	TEnumParameter(e1:TypedExpr, ef:EnumField, index:Int);

	/**
		Access to an enum index (generated by the pattern matcher).
	**/
	TEnumIndex(e1:TypedExpr);

	/**
		An unknown identifier.
	**/
	TIdent(s:String);
}

/**
	Represents a variable in the typed AST.
 */
typedef TVar = {
	/**
		The unique ID of the variable.
	**/
	public var id(default, never):Int;

	/**
		The name of the variable.
	**/
	public var name(default, never):String;

	/**
		The type of the variable.
	**/
	public var t(default, never):Type;

	/**
		Whether or not the variable has been captured by a closure.
	**/
	public var capture(default, never):Bool;

	/**
		Special information which is internally used to keep track of closure.
		information
	**/
	public var extra(default, never):Null<{params:Array<TypeParameter>, expr:Null<TypedExpr>}>;

	/**
		The metadata of the variable.
	**/
	public var meta(default, never):Null<MetaAccess>;

	/**
		Whether the variable is a local static variable
	**/
	public var isStatic(default, never):Bool;
}

/**
	Represents a typed AST node.
 */
typedef TypedExpr = {
	/**
		The expression kind.
	**/
	var expr:TypedExprDef;

	/**
		The position of the expression.
	**/
	var pos:Expr.Position;

	/**
		The type of the expression.
	**/
	var t:Type;
}
