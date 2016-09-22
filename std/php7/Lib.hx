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
package php7;

/**
	Platform-specific PHP Library. Provides some platform-specific functions
	for the PHP target, such as conversion from Haxe types to native types
	and vice-versa.
**/
class Lib {
	/**
		Print the specified value on the default output.
	**/
	public static function print( v : Dynamic ) : Void {
		untyped __call__("echo", Std.string(v));
	}

	/**
		Print the specified value on the default output followed by
		a newline character.
	**/
	public static function println( v : Dynamic ) : Void {
		print(v);
		print("\n");
	}

	/**
		Displays structured information about one or more expressions
		that includes its type and value. Arrays and objects are
		explored recursively with values indented to show structure.
	*/
	public static function dump(v : Dynamic) : Void {
		untyped __call__("var_dump", v);
	}

	/**
		Serialize using native PHP serialization. This will return a binary
		`String` that can be stored for long term usage.
	**/
	public static function serialize( v : Dynamic ) : String {
		return untyped __call__("serialize", v);
	}

	/**
		Unserialize a `String` using native PHP serialization. See `php.Lib.serialize()`.
	**/
	public static function unserialize( s : String ) : Dynamic {
		return untyped __call__("unserialize", s);
	}

	/**
		Find out whether an extension is loaded.
	*/
	public static function extensionLoaded(name : String) {
		return untyped __call__("extension_loaded", name);
	}

	public static function isCli() : Bool {
		return untyped __php__("(0 == strncasecmp(PHP_SAPI, 'cli', 3))");
	}

	/**
		Output file content from the given file name.
	*/
	public static function printFile(file : String) {
		return untyped __call__("fpassthru", __call__("fopen", file,  "r"));
	}

	public static inline function toPhpArray(a : Array<Dynamic>) : NativeArray {
		return @:privateAccess a.arr;
	}

	public static inline function toHaxeArray(a : NativeArray) : Array<Dynamic> {
		return @:privateAccess Array.wrap(a);
	}

	public static function hashOfAssociativeArray<T>(arr : NativeAssocArray<T>) : Map<String,T> {
		var result = new Map();
		for (key in Global.array_keys(arr)) {
			result.set(key, arr[key]);
		}
		return result;
	}

	public static function associativeArrayOfHash(hash : haxe.ds.StringMap<Dynamic>) : NativeArray {
		throw "Not implemented";
	}

	public static function objectOfAssociativeArray(arr : NativeArray) : Dynamic {
		throw "Not implemented";
	}

	public static function associativeArrayOfObject(ob : Dynamic) : NativeArray {
		throw "Not implemented";
	}

	/**
	 * See the documentation for the equivalent PHP function for details on usage:
	 * <http://php.net/manual/en/function.mail.php>
	 * @param	to
	 * @param	subject
	 * @param	message
	 * @param	?additionalHeaders
	 * @param	?additionalParameters
	 */
	public static function mail(to : String, subject : String, message : String, ?additionalHeaders : String, ?additionalParameters : String) : Bool
	{
		throw "Not implemented";
	}

	/**
		For neko compatibility only.
	**/
	public static function rethrow( e : Dynamic ) {
		throw "Not implemented";
	}

	static function appendType(o : Dynamic, path : Array<String>, t : Dynamic) {
		throw "Not implemented";
	}

	public static function getClasses() {
		throw "Not implemented";
	}

	/**
	*  Loads types defined in the specified directory.
 	*/
 	public static function loadLib(pathToLib : String) : Void
 	{
		throw "Not implemented";
 	}
}
