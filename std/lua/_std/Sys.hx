
/*
 * Copyright (C)2005-2016 Haxe Foundation
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
@:coreApi
class Sys {
	public static inline function print( v : Dynamic ) : Void {
		return lua.Lib.print(v);
	}
	public static inline function println( v : Dynamic ) : Void {
		return lua.Lib.println(v);
	}
	public inline static function args() : Array<String> {
		return lua.Boot.tableToArray(lua.Lua.arg);
	}
	public inline static function command( cmd : String, ?args : Array<String> ) : Int  {
		return lua.Os.execute('$cmd ${args.join(" ")}');
	}
	public inline static function cpuTime() : Float {
		return lua.Os.clock();
	}

	public inline static function exit(code : Int) : Void {
		lua.Os.exit(code);
	}

	public inline static function getChar(echo : Bool) : Int {
		return lua.NativeStringTools.byte(lua.Io.read(1));
	}

	// TODO
	public inline static function environment() : Map<String,String>  return new Map();

	// TODO
	public inline static function executablePath() : String return null;

	// TODO
	public static function getCwd() : String return null;
	// TODO
	public static function setCwd(s : String) : Void null;

	public static function getEnv(s : String) : String return lua.Os.getenv(s);
	// TODO
	public static function putEnv(s : String, v : String ) : Void null;


	// TODO verify
	public static function setTimeLocale(loc : String) : Bool  return lua.Os.setlocale(loc) != null;

	// TODO
	public static function sleep(seconds : Float) : Void null;

	// TODO
	public static function stderr() : haxe.io.Output return null;
	public static function stdin() : haxe.io.Input return null;
	public static function stdout() : haxe.io.Output return null;

	// TODO
	public static function systemName() : String return null;

	// TODO
	public static function time() : Float return lua.Os.time();


}
