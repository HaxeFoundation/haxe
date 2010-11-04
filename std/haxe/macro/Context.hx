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
import haxe.macro.Expr;

/**
	This is an API that can be used by macros implementations.
**/
class Context {

#if neko
	/**
		Display a compilation error at the given position in code
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
		Returns the position at the place the macro is called
	**/
	public static function currentPos() : Position {
		return load("curpos", 0)();
	}

	/**
		Tells is the given compiler directive has been defined with -D
	**/
	public static function defined( s : String ) : Bool {
		return load("defined", 1)(untyped s.__s);
	}

	/**
		Resolve a type from its name.
	**/
	public static function getType( name : String ) : Type {
		return load("get_type", 1)(untyped name.__s);
	}

	static function load( f, nargs ) : Dynamic {
		#if macro
		return neko.Lib.load("macro", f, nargs);
		#else
		return Reflect.makeVarArgs(function(_) throw "Can't be called outside of macro");
		#end
	}

#end

}