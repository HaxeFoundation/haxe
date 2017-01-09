/*
 * Copyright (C)2005-2017 Haxe Foundation
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

/**
	Platform-specific Cpp Library. Provides some platform-specific functions 
	for the C++ target, such as conversion from Haxe types to native types 
	and vice-versa.
**/
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
		Unloaded all dynamic libraries in reverse order of loading.
		Returns the number of libraries unloaded.
	**/
	public static function unloadAllLibraries() : Int {
		return untyped __global__.__hxcpp_unload_all_libraries();
	}

	public static function _loadPrime( lib : String, prim : String, signature : String, quietFail = false ) : Dynamic {
		var factory:Callable< ConstCharStar -> Object > =
		untyped __global__.__hxcpp_cast_get_proc_address(lib, prim + "__prime", quietFail);
		if (factory!=null)
		{
			var func:Dynamic = factory.call(signature);
			if (func==null && !quietFail)
				throw '$prim does not have signature $signature';
			return func;
		}
		return null;
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

	@:extern  @:noDebug @:native("HX_STACK_DO_RETHROW")
	static function do_rethrow(inExp:Dynamic) { throw inExp; }

	@:noDebug #if(!cppia) inline #end
	public static function rethrow(inExp:Dynamic) { do_rethrow(inExp); }

	public static function stringReference(inBytes:haxe.io.Bytes) : String
	{
		var result:String = "";
		untyped __global__.__hxcpp_string_of_bytes(inBytes.b, result, 0, 0, true);
		return result;
	}

	public static function pushDllSearchPath(inPath:String) : Void
		untyped __global__.__hxcpp_push_dll_path(inPath);

	public static function getDllExtension() : String
		return untyped __global__.__hxcpp_get_dll_extension();

	public static function getBinDirectory() : String
		return untyped __global__.__hxcpp_get_bin_dir();

	/**
		Returns bytes referencing the content of a string.
		Use with extreme caution - changing constant strings will crash.
		Changing one string can cause others to change unexpectedly.
		Only really safe if you are using it read-only or if it comes from stringReference above
	**/
	public inline static function bytesReference( s : String ) : haxe.io.Bytes {
		var bytes = new haxe.io.BytesData();
		untyped bytes.__unsafeStringReference(s);
		return haxe.io.Bytes.ofData(bytes);
	}

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

	public static function setFloatFormat(inFormat:String):Void
	{
		untyped __global__.__hxcpp_set_float_format(inFormat);
	}

}
