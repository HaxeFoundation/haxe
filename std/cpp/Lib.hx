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
package cpp;

class Lib {

	/**
		Load and return a Cpp primitive from a DLL library.
	**/
	public static function load( lib : String, prim : String, nargs : Int ) : Dynamic {
		#if (iphone || emscripten)
		return loadLazy(lib,prim,nargs);
		#else
		return untyped __global__.__loadprim(lib,prim,nargs);
		#end
	}

	/**
		Load and return a Cpp primitive from a DLL library.
	**/
	@:extern public static inline function getProcAddress( lib : String, prim : String ) : Dynamic {
		return untyped __global__.__hxcpp_cast_get_proc_address(lib,prim);
	}

	/**
		Tries to load, and always returns a valid function, but the function may throw
		if called.
	**/
	public static function loadLazy(lib,prim,nargs) : Dynamic {
		try {
			return untyped __global__.__loadprim(lib,prim,nargs);
		} catch( e : Dynamic ) {
			switch(nargs) {
			case 0 : return function() { throw e; };
			case 2 : return function(_1,_2) { throw e; };
			case 3 : return function(_1,_2,_3) { throw e; };
			case 4 : return function(_1,_2,_3,_4) { throw e; };
			case 5 : return function(_1,_2,_3,_4,_5) { throw e; };
			default : return function(_1) { throw e; };
			}
		}
		return null;
	}

	public static function rethrow(inExp:Dynamic) { throw inExp; }

	public static function stringReference(inExp:Dynamic) { throw inExp; }

	/**
		Print the specified value on the default output.
	**/
	public static function print( v : Dynamic ) : Void {
		untyped __global__.__hxcpp_print(v);
	}

	/**
		This function is used to make porting from neko to cpp easy.
		It does not need to do anything because the c-code can work with any Dynamic
	**/
	public static function haxeToNeko( v : Dynamic ) : Dynamic {
		return v;
	}

	/**
		This function is used to make porting from neko to cpp easy.
		It does not need to do anything because the c-code can work with any Dynamic
	**/
	public static function nekoToHaxe( v : Dynamic ) : Dynamic {
		return v;
	}
	/**
		Print the specified value on the default output followed by a newline character.
	**/
	public static function println( v : Dynamic ) : Void {
		untyped __global__.__hxcpp_println(v);
	}

}
