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
package php;

import haxe.ds.StringMap;

/**
	Platform-specific PHP Library. Provides some platform-specific functions
	for the PHP target, such as conversion from Haxe types to native types
	and vice-versa.
**/
class Lib {
	/**
		Print the specified value on the default output.
	**/
	public static inline function print( v : Dynamic ) : Void {
		Global.echo(Std.string(v));
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
	public static inline function dump(v : Dynamic) : Void {
		Global.var_dump(v);
	}

	/**
		Serialize using native PHP serialization. This will return a binary
		`String` that can be stored for long term usage.
	**/
	public static inline function serialize( v : Dynamic ) : String {
		return Global.serialize(v);
	}

	/**
		Unserialize a `String` using native PHP serialization. See `php.Lib.serialize()`.
	**/
	public static inline function unserialize( s : String ) : Dynamic {
		return Global.unserialize(s);
	}

	/**
		Find out whether an extension is loaded.
	*/
	public static inline function extensionLoaded(name : String) {
		return Global.extension_loaded(name);
	}

	public static inline function isCli() : Bool {
		return 0 == Global.strncasecmp(Const.PHP_SAPI, 'cli', 3);
	}

	/**
		Output file content from the given file name.
	*/
	public static inline function printFile(file : String) {
		return Global.fpassthru(Global.fopen(file,  "r"));
	}

	public static inline function toPhpArray(a : Array<Dynamic>) : NativeArray {
		return @:privateAccess a.arr;
	}

	public static inline function toHaxeArray(a : NativeArray) : Array<Dynamic> {
		return @:privateAccess Array.wrap(a);
	}

	public static function hashOfAssociativeArray<T>(arr : NativeAssocArray<T>) : Map<String,T> {
		var result = new StringMap();
		@:privateAccess result.data = arr;
		return result;
	}

	public static inline function associativeArrayOfHash(hash : haxe.ds.StringMap<Dynamic>) : NativeArray {
		return @:privateAccess hash.data;
	}

	public static inline function objectOfAssociativeArray(arr : NativeArray) : Dynamic {
		return Boot.createAnon(arr);
	}

	public static inline function associativeArrayOfObject(ob : Dynamic) : NativeArray {
		return Syntax.array(ob);
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
