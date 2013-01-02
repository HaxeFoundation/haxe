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

/**
	This is an API that can be used by macros implementations.
**/
#if !neko @:noDoc #end
class Context {

#if neko
	/**
		Display a compilation error at the given position in code and abort the current macro call
	**/
	public static function error( msg : String, pos : Position ) : Dynamic {
		return load("error",2)(untyped msg.__s, pos);
	}

	/**
		Display a compilation warning at the given position in code
	**/
	public static function warning( msg : String, pos : Position ) {
		load("warning",2)(untyped msg.__s, pos);
	}

	/**
		Resolve a filename based on current classpath.
	**/
	public static function resolvePath( file : String ) {
		return new String(load("resolve",1)(untyped file.__s));
	}

	/**
		Return the current classpath
	**/
	public static function getClassPath() : Array<String> {
		var c : neko.NativeArray<neko.NativeString> = load("class_path",0)();
		var a = new Array();
		for( i in 0...neko.NativeArray.length(c) )
			a.push(Std.string(c[i]));
		return a;
	}

	/**
		Returns the position at which the macro is called
	**/
	public static function currentPos() : Position {
		return load("curpos", 0)();
	}

	/**
		Returns the current class in which the macro is called
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
		Returns the current type in/on which the macro is called
	**/
	public static function getLocalType() : Null<Type> {
		var l : Type = load("local_type", 0)();
		if( l == null ) return null;
		return l;
	}

	/**
		Returns the name of the method from which the macro was called
	**/
	public static function getLocalMethod() : Null<String> {
		var l : String = load("local_method", 0)();
		if (l == "") return null;
		return l;
	}

	/**
		Returns classes which are available for "using" where the macro was called
	**/	
	public static function getLocalUsing() :  Array<Type.Ref<Type.ClassType>> {
		return load("local_using", 0)();
	}
	
	/**
		Returns local variables accessible where the macro was called
	**/
	public static function getLocalVars() : Hash<Type> {
		return load("local_vars", 0)();
	}

	/**
		Tells is the given compiler directive has been defined with -D
	**/
	public static function defined( s : String ) : Bool {
		return load("defined", 1)(untyped s.__s);
	}
	
	/**
		Returns the value defined through -D key=value
	**/
	public static function definedValue( key : String ) : String {
		return load("defined_value", 1)(untyped key.__s);
	}	

	/**
		Resolve a type from its name.
	**/
	public static function getType( name : String ) : Type {
		return load("get_type", 1)(untyped name.__s);
	}

	/**
		Return the list of types defined in the given compilation unit module
	**/
	public static function getModule( name : String ) : Array<Type> {
		return load("get_module", 1)(untyped name.__s);
	}

	/**
		Parse a constructed string into the corresponding expression.
	**/
	public static function parse( expr : String, pos : Position ) : Expr {
		return load("parse", 3)(untyped expr.__s, pos, false);
	}

	/**
		Parse a string contained into source code into the corresponding expression. Errors positions are reported within this string
	**/
	public static function parseInlineString( expr : String, pos : Position ) : Expr {
		return load("parse", 3)(untyped expr.__s, pos, true);
	}

	/**
		Build an expression corresponding to the given runtime value. Only basic types + enums are supported.
	**/
	public static function makeExpr( v : Dynamic, pos : Position ) : Expr {
		return load("make_expr", 2)(v, pos);
	}

	/**
		Quickly build an hashed MD5 signature for any given value
	**/
	public static function signature( v : Dynamic ) : String {
		return new String(load("signature", 1)(v));
	}

	/**
		Set a callback function that will return all the types compiled before they get generated.
	**/
	public static function onGenerate( callb : Array<Type> -> Void ) {
		load("on_generate",1)(callb);
	}
	
	/**
		Set a callback function that will be called when a type cannot be found.
	**/
	public static function onTypeNotFound ( callb : String -> TypeDefinition ) {
		load("on_type_not_found",1)(callb);
	}

	/**
		Evaluate the type a given expression would have in the context of the current macro call.
	**/
	public static function typeof( e : Expr ) : Type {
		return load("typeof", 1)(e);
	}

	/**
		Returns the ComplexType corresponding to the given Type.
	**/
	public static function toComplexType( t : Type ) : Null<ComplexType> {
		return load("to_complex", 1)(t);
	}
	
	/**
		Returns true if t1 and t2 unify, false otherwise
	**/
	public static function unify( t1 : Type, t2 : Type) : Bool {
		return load("unify", 2)(t1, t2);
	}
	
	/**
		Follow all typedefs to reach the actual real type
	**/
	public static function follow( t : Type, ?once : Bool ) : Type {
		return load("follow", 2)(t,once);
	}

	/**
		Get the informations stored into a given position.
	**/
	public static function getPosInfos( p : Position ) : { min : Int, max : Int, file : String } {
		var i = load("get_pos_infos",1)(p);
		i.file = new String(i.file);
		return i;
	}

	/**
		Build a position with the given informations.
	**/
	public static function makePosition( inf : { min : Int, max : Int, file : String } ) : Position {
		return load("make_pos",3)(inf.min,inf.max,untyped inf.file.__s);
	}

	/**
		Add or modify a resource that will be accessible with haxe.Resource api.
	**/
	public static function addResource( name : String, data : haxe.io.Bytes ) {
		load("add_resource",2)(untyped name.__s,data.getData());
	}

	/**
		Returns the list of fields for the current type inside the build macro.
	**/
	public static function getBuildFields() : Array<Field> {
		return load("build_fields", 0)();
	}

	/**
		Define a new type based on the given definition.
	**/
	public static function defineType( t : TypeDefinition ) : Void {
		load("define_type", 1)(t);
	}


	/**
		Return the raw expression corresponding to the given typed expression.
	**/
	public static function getTypedExpr( t : Type.TypedExpr ) : Expr {
		return load("get_typed_expr",1)(t);
	}

	/**
		Manually add a dependency between a module and a third party file :
		make sure the module gets recompiled (if it was cached) in case the extern file has been modified as well.
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

	@:allow(haxe.macro.TypeTools)
	static function load( f, nargs ) : Dynamic {
		#if macro
		return neko.Lib.load("macro", f, nargs);
		#else
		return Reflect.makeVarArgs(function(_) return throw "Can't be called outside of macro");
		#end
	}

#end

}