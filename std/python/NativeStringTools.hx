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
package python;

import python.Bytes;
import python.Tuple;

class NativeStringTools {

	public static function format (s:String, args:Array<Dynamic>):String
	{
		return python.Syntax.field(s, "format")(python.Syntax.varArgs(args));
	}

	public static inline function encode(s:String, encoding:String="utf-8", errors:String="strict"):Bytes {
		return (python.Syntax.callField(s, "encode", encoding, errors):Bytes);
	}

	public static inline function contains(s:String, e:String):Bool {
		return python.Syntax.isIn(e,s);
	}

	public static inline function strip(s:String, ?chars:String):String
	{
		return python.Syntax.field(s, "strip")(chars);
	}

	public static inline function rpartition (s:String, sep:String):Tuple3<String, String, String>
	{
		return python.Syntax.field(s, "rpartition")(sep);
	}

}