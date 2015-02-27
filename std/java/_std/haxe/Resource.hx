/*
 * Copyright (C)2005-2014 Haxe Foundation
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
package haxe;

@:coreApi class Resource {

	@:keep static var content : Array<String>;

	public static inline function listNames() : Array<String> {
		return content.copy();
	}

	private static function unescapeName( name : String ) : String
	{
		var regex = ~/-x([0-9]+)/g;
		return regex.map(name, function(regex) return String.fromCharCode(Std.parseInt(regex.matched(1))));
	}

	private static function escapeName( name : String ) : String
	{
		var regex = ~/[^A-Za-z0-9_\/]/g;
		return regex.map(name, function(v) return '-x' + v.matched(0).charCodeAt(0));
	}

	public static function getString( name : String ) : String {
		name = escapeName(name);
		var stream = cast(Resource, java.lang.Class<Dynamic>).getResourceAsStream("/" + name);
		if (stream == null)
			return null;
		var stream = new java.io.NativeInput(stream);
		return stream.readAll().toString();
	}

	public static function getBytes( name : String ) : haxe.io.Bytes {
		name = escapeName(name);
		var stream = cast(Resource, java.lang.Class<Dynamic>).getResourceAsStream("/" + name);
		if (stream == null)
			return null;
		var stream = new java.io.NativeInput(stream);
		return stream.readAll();
	}
}
