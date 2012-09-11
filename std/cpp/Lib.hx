/*
 * Copyright (c) 2005, The haXe Project Contributors
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
package cpp;

class Lib {

	/**
		Load and return a Cpp primitive from a DLL library.
	**/
	public static function load( lib : String, prim : String, nargs : Int ) : Dynamic {
		#if iphone
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
