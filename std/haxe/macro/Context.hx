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

import haxe.macro.Expr;
import haxe.macro.Type.TypedExpr;

/**
	Context provides an API for macro programming.

	It contains common functions that interact with the macro interpreter to
	query or set information. Other API functions are available in the tools
	classes:

	- `haxe.macro.ComplexTypeTools`
	- `haxe.macro.ExprTools`
	- `haxe.macro.TypeTools`
**/
#if !neko @:noDoc #end
class Context {

#if neko
	/**
		Displays a compilation error `msg` at the given `Position` `pos`
		and aborts the current macro call.
	**/
	public static function error( msg : String, pos : Position ) : Dynamic {
		return load("error",2)(untyped msg.__s, pos);
	}

	/**
		Displays a compilation error `msg` at the given `Position` `pos`
		and aborts the compilation.
	**/
	public static function fatalError( msg : String, pos : Position ) : Dynamic {
		return load("fatal_error",2)(untyped msg.__s, pos);
	}

	/**
		Displays a compilation warning `msg` at the given `Position` `pos`.
	**/
	public static function warning( msg : String, pos : Position ) {
		load("warning",2)(untyped msg.__s, pos);
	}

	/**
		Resolves a file name `file` based on the current class paths.

		The resolution follows the usual class path rules where the last
		declared class path has priority.

		If a class path was declared relative, this method returns the relative
		file path. Otherwise it returns the absolute file path.
	**/
	public static function resolvePath( file : String ) {
		return new String(load("resolve",1)(untyped file.__s));
	}

	/**
		Returns an `Array` of current class paths in the order of their
		declaration.

		Modifying the returned array has no effect on the compiler. Class paths
		can be added using `haxe.macro.Compiler.addClassPath`.
	**/
	public static function getClassPath() : Array<String> {
		var c : neko.NativeArray<neko.NativeString> = load("class_path",0)();
		var a = new Array();
		for( i in 0...neko.NativeArray.length(c) )
			a.push(Std.string(c[i]));
		return a;
	}

	/**
		Returns the position at which the macro was called.
	**/
	public static function currentPos() : Position {
		return load("curpos", 0)();
	}

	/**
		Returns the type which is expected at the place the macro is called.

		This affects usages such as `var x:Int = macroCall()`, where the
		expected type will be reported as Int.

		Might return null if no specific type is expected or if the calling
		macro is not an expression-macro.
	**/
	@:require(haxe_ver >= 3.1)
	public static function getExpectedType():Null<Type> {
		var l : Type = load("expected_type", 0)();
		if( l == null ) return null;
		return l;
	}

	/**
		Returns the call arguments that lead to the invocation of the current
		`@:genericBuild` macro, if available.

		Returns `null` if the current macro is not a `@:genericBuild` macro.
	**/
	@:require(haxe_ver >= 3.2)
	public static function getCallArguments():Null<Array<Expr>> {
		return load("call_arguments", 0)();
	}

	/**
		Returns the current class in which the macro was called.

		If no such class exists, null is returned.
	**/
	public static function getLocalClass() : Null<Type.Ref<Type.ClassType>> {
		var l : Type = load("local_type", 0)();
		if( l == null ) return null;
		return switch( l ) {
		case TInst(c,_): c;
		default: null;
		}
	}

	/**
		Returns the current module path in/on which the macro was called.
	**/
	public static function getLocalModule() : String {
		return new String(load("local_module", 0)());
	}

	/**
		Returns the current type in/on which the macro was called.

		If no such type exists, null is returned.
	**/
	public static function getLocalType() : Null<Type> {
		var l : Type = load("local_type", 0)();
		if( l == null ) return null;
		return l;
	}

	/**
		Returns the name of the method from which the macro was called.

		If no such method exists, null is returned.
	**/
	public static function getLocalMethod() : Null<String> {
		var l : String = load("local_method", 0)();
		if (l == "") return null;
		return new String(l);
	}

	/**
		Returns an `Array` of classes which are available for `using` usage in
		the context the macro was called.

		Modifying the returned array has no effect on the compiler.
	**/
	public static function getLocalUsing() :  Array<Type.Ref<Type.ClassType>> {
		return load("local_using", 0)();
	}

	/**
		Returns an `Array` of all imports in the context the macro was called.

		Modifying the returned array has no effect on the compiler.
	**/
	public static function getLocalImports() :  Array<ImportExpr> {
		return load("local_imports", 0)();
	}

	/**
		Returns a map of local variables accessible in the context the macro was
		called.

		The keys of the returned map are the variable names, the values are
		their types.

		Modifying the returned map has no effect on the compiler.
	**/
	@:deprecated("Use Context.getLocalTVars() instead")
	public static function getLocalVars() : Map<String,Type> {
		return load("local_vars", 1)(false);
	}

	/**
		Similar to `getLocalVars`, but returns elements of type `TVar` instead
		of `Type`.
	**/
	@:require(haxe_ver >= 3.102)
	public static function getLocalTVars() : Map<String,Type.TVar> {
		return load("local_vars", 1)(true);
	}

	/**
		Tells if compiler directive `s` has been set.

		Compiler directives are set using the `-D` command line parameter, or
		by calling `haxe.macro.Compiler.define`.
	**/
	public static function defined( s : String ) : Bool {
		return load("defined", 1)(untyped s.__s);
	}

	/**
		Returns the value defined for compiler directive `key`.

		If no value is defined for `key`, null is returned.

		Compiler directive values are set using the `-D key=value` command line
		parameter, or by calling `haxe.macro.Compiler.define`.

		The default value is `"1"`.
	**/
	public static function definedValue( key : String ) : String {
		var d = load("defined_value", 1)(untyped key.__s);
		return d == null ? null : new String(d);
	}

	/**
		Returns a map of all compiler directives that have been set.

		Compiler directives are set using the `-D` command line parameter, or
		by calling `haxe.macro.Compiler.define`.

		Modifying the returned map has no effect on the compiler.
	 */
	public static function getDefines() : Map<String,String> {
		return load("get_defines", 0)();
	}

	/**
		Resolves a type identified by `name`.

		The resolution follows the usual class path rules where the last
		declared class path has priority.

		If no type can be found, an exception of type `String` is thrown.
	**/
	public static function getType( name : String ) : Type {
		return load("get_type", 1)(untyped name.__s);
	}

	/**
		Resolves a module identified by `name` and returns an `Array` of all
		its contained types.

		The resolution follows the usual class path rules where the last
		declared class path has priority.

		If no module can be found, null is returned.
	**/
	public static function getModule( name : String ) : Array<Type> {
		return load("get_module", 1)(untyped name.__s);
	}

	/**
		Parses `expr` as haxe code, returning the corresponding AST.

		String interpolation of single quote strings within `expr` is not
		supported.

		The provided `Position` `pos` is used for all generated inner AST nodes.
	**/
	public static function parse( expr : String, pos : Position ) : Expr {
		return load("parse", 3)(untyped expr.__s, pos, false);
	}

	/**
		Similar to `parse`, but error positions are reported within the provided
		String `expr`.
	**/
	public static function parseInlineString( expr : String, pos : Position ) : Expr {
		return load("parse", 3)(untyped expr.__s, pos, true);
	}

	/**
		Builds an expression from `v`.

		This method generates AST nodes depending on the macro-runtime value of
		`v`. As such, only basic types and enums are supported and the behavior
		for other types is undefined.

		The provided `Position` `pos` is used for all generated inner AST nodes.
	**/
	public static function makeExpr( v : Dynamic, pos : Position ) : Expr {
		return load("make_expr", 2)(v, pos);
	}

	/**
		Returns a hashed MD5 signature of value `v`.
	**/
	public static function signature( v : Dynamic ) : String {
		return new String(load("signature", 1)(v));
	}

	/**
		Adds a callback function `callback` which is invoked after the
		compiler's typing phase, just before its generation phase.

		The callback receives an `Array` containing all types which are about
		to be generated. Modifications are limited to metadata, it is mainly
		intended to obtain information.
	**/
	public static function onGenerate( callback : Array<Type> -> Void ) {
		load("on_generate",1)(callback);
	}

	/**
		Adds a callback function `callback` which is invoked after the compiler
		generation phase.

		Compilation has completed at this point and cannot be influenced
		anymore. However, contextual information is still available.
	**/
	@:require(haxe_ver >= 3.1)
	public static function onAfterGenerate( callback : Void -> Void ) {
		load("after_generate",1)(callback);
	}

	/**
		Adds a callback function `callback` which is invoked when a type name
		cannot be resolved.

		The callback may return a type definition, which is then used for the
		expected type. If it returns null, the type is considered to still not
		exist.
	**/
	public static function onTypeNotFound ( callback : String -> TypeDefinition ) {
		load("on_type_not_found",1)(callback);
	}

	/**
		Types expression `e` and returns its type.

		Typing the expression may result in a compiler error which can be
		caught using `try ... catch`.
	**/
	public static function typeof( e : Expr ) : Type {
		return load("typeof", 1)(e);
	}

	/**
		Types expression `e` and returns the corresponding `TypedExpr`.

		Typing the expression may result in a compiler error which can be
		caught using `try ... catch`.
	**/
	@:require(haxe_ver >= 3.1)
	public static function typeExpr( e : Expr ) : TypedExpr {
		return load("type_expr", 1)(e);
	}

	/**
		Returns the `ComplexType` corresponding to the given `Type` `t`.

		See `haxe.macro.TypeTools.toComplexType` for details.
	**/
	public static function toComplexType( t : Type ) : Null<ComplexType> {
		return load("to_complex", 1)(t);
	}

	/**
		Tries to unify `t1` and `t2` and returns `true` if successful.
	**/
	public static function unify( t1 : Type, t2 : Type) : Bool {
		return load("unify", 2)(t1, t2);
	}

	/**
		Follows a type.

		See `haxe.macro.TypeTools.follow` for details.
	**/
	public static function follow( t : Type, ?once : Bool ) : Type {
		return load("follow", 2)(t,once);
	}

	/**
		Returns the information stored in `Position` `p`.
	**/
	public static function getPosInfos( p : Position ) : { min : Int, max : Int, file : String } {
		var i = load("get_pos_infos",1)(p);
		i.file = new String(i.file);
		return i;
	}

	/**
		Builds a `Position` from `inf`.
	**/
	public static function makePosition( inf : { min : Int, max : Int, file : String } ) : Position {
		return load("make_pos",3)(inf.min,inf.max,untyped inf.file.__s);
	}

	/**
		Returns a map of all registered resources for this compilation unit.

		Modifying the returned map has no effect on the compilation, use
		`haxe.macro.Context.addResource` to add new resources to the compilation unit.
	**/
	public static function getResources():Map<String,haxe.io.Bytes> {
		var x:haxe.ds.StringMap<neko.NativeString> = load("get_resources",0)();
		var r = new haxe.ds.StringMap();
		for (k in x.keys()) {
			r.set(k, haxe.io.Bytes.ofData(x.get(k)));
		}
		return r;
	}

	/**
		Makes resource `data` available as `name`.

		The resource is then available using the `haxe.macro.Resource` API.

		If a previous resource was bound to `name`, it is overwritten.
	**/
	public static function addResource( name : String, data : haxe.io.Bytes ) {
		load("add_resource",2)(untyped name.__s,data.getData());
	}

	/**
		Returns an `Array` of fields of the class which is to be built.

		This is only defined for `@:build/@:autoBuild` macros.
	**/
	public static function getBuildFields() : Array<Field> {
		return load("build_fields", 0)();
	}

	/**
		Defines a new type from `TypeDefinition` `t`.
	**/
	public static function defineType( t : TypeDefinition ) : Void {
		load("define_type", 1)(t);
	}

	/**
		Defines a new module as `modulePath` with several `TypeDefinition`
		`types`. This is analogous to defining a .hx file.

		The individial `types` can reference each other and any identifier
		respects the `imports` and `usings` as usual, expect that imports are
		not allowed to have `.*` wildcards or `in s` shorthands.
	**/
	public static function defineModule( modulePath : String, types : Array<TypeDefinition>, ?imports: Array<ImportExpr>, ?usings : Array<TypePath> ) : Void {
		if (imports == null) imports = [];
		if (usings == null) usings = [];
		load("define_module", 4)(untyped modulePath.__s, untyped types.__neko(), untyped imports.__neko(), untyped usings.__neko());
	}

	/**
		Returns a syntax-level expression corresponding to typed expression `t`.

		This process may lose some information.
	**/
	public static function getTypedExpr( t : Type.TypedExpr ) : Expr {
		return load("get_typed_expr",1)(t);
	}


	/**
		Store typed expression `t` internally and give a syntax-level expression
		that can be returned from a macro and will be replaced by the stored
		typed expression.

		If `t` is null or invalid, an exception is thrown.

		NOTE: the returned value references an internally stored typed expression
		that is reset between compilations, so care should be taken when storing
		the expression returned by this method in a static variable and using the
		compilation server.
	**/
	@:require(haxe_ver >= 3.2)
	public static function storeTypedExpr( t : Type.TypedExpr ) : Expr {
		return load("store_typed_expr",1)(t);
	}

	/**
		Manually adds a dependency between module `modulePath` and an external
		file `externFile`.

		This affects the compilation cache, causing the module to be typed if
		`externFile` has changed.

		Has no effect if the compilation cache is not used.
	**/
	public static function registerModuleDependency( modulePath : String, externFile : String ) {
		load("module_dependency", 2)(untyped modulePath.__s,untyped externFile.__s);
	}

	/**
		Add a macro call to perform in case the module is reused by the compilation cache.
	**/
	public static function registerModuleReuseCall( modulePath : String, macroCall : String ) {
		load("module_reuse_call", 2)(untyped modulePath.__s,untyped macroCall.__s);
	}

	/**
		Register a callback function that will be called everytime the macro context cached is reused with a new
		compilation. This enable to reset some static vars since the code might have been changed. If the callback
		returns false, the macro context is discarded and another one is created.
	**/
	public static function onMacroContextReused( callb : Void -> Bool ) {
		load("macro_context_reused", 1)(callb);
	}

	@:allow(haxe.macro.TypeTools)
	@:allow(haxe.macro.MacroStringTools)
	@:allow(haxe.macro.TypedExprTools)
	static function load( f, nargs ) : Dynamic {
		#if macro
		return neko.Lib.load("macro", f, nargs);
		#else
		return Reflect.makeVarArgs(function(_) return throw "Can't be called outside of macro");
		#end
	}

#end

}
