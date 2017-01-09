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
package lua;
import lua.Lua;
import lua.Io;
import lua.NativeStringTools;

/**
	Platform-specific Lua Library. Provides some platform-specific functions 
	for the Lua target, such as conversion from Haxe types to native types 
	and vice-versa.
**/
class Lib {
	/**
		Print the specified value on the default output followed by a newline character.
	**/
	public static inline function println( v : Dynamic ) : Void {
		Lua.print(Std.string(v));
	}

	/**
		Print the specified value on the default output.
	**/
	public static inline function print(v:Dynamic) : Void {
		Io.write(Std.string(v));
		Io.flush();
	}

	public inline static function tableToArray<T>(t:Table<Int,T>, ?length:Int) : Array<T> {
		return Boot.defArray(t, length);
	}

	public inline static function tableToObject<T>(t:Table<String,T>) : Dynamic<T> {
		return Boot.tableToObject(t);
	}

	public inline static function patternQuote(str:String) : String {
		return NativeStringTools.gsub(str, "[%(%)%.%%%+%-%*%?%[%]%^%$]", function(c:String){ return "%" + c; });
	}

	public inline static function defArray<T>(tab: Table<Int,T>, length : Int) : Array<T> {
		return Boot.defArray(tab, length);
	}

	public static function fillArray<T>(itr:Void->T) : Array<T> {
		var i: T = null;
		var ret : Array<T> = [];
		while({i = itr(); i != null;}){
			ret.push(i);
		}
		return ret;
	}

	public static function isShellAvailable() : Bool {
		var ret : Dynamic = Os.execute();
		if (Lua.type(ret) == "bool"){
			return ret;
		} else {
			return ret != 0;
		}
	}
}
