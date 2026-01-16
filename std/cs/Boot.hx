/*
 * Copyright (C)2005-2019 Haxe Foundation
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

package cs;

@:keep
class Boot {
	public static function toString(obj:Dynamic):String {
		if (obj == null) {
			return "null";
		}
		// Use __cs__ to call ToString directly since it exists on all C# objects
		return untyped __cs__("{0}.ToString()", obj);
	}

	public static function parseInt(s:String, radix:Int):Int {
		// Will be implemented via C# native call
		return untyped __cs__("int.Parse({0}, {1} == 16 ? System.Globalization.NumberStyles.HexNumber : System.Globalization.NumberStyles.Integer, System.Globalization.CultureInfo.InvariantCulture)", s, radix);
	}

	public static function parseFloat(s:String):Float {
		// Will be implemented via C# native call
		return untyped __cs__("double.Parse({0}, System.Globalization.CultureInfo.InvariantCulture)", s);
	}

	public static function trace(v:Dynamic, ?infos:haxe.PosInfos):Void {
		var str = toString(v);
		if (infos != null) {
			// Use _hx_getField for field access on anonymous objects (PosInfos is an anonymous type)
			str = untyped __cs__("((haxe.root.HaxeDynamicObject){0})._hx_getField(\"fileName\")", infos) + ":"
				+ untyped __cs__("((haxe.root.HaxeDynamicObject){0})._hx_getField(\"lineNumber\")", infos) + ": " + str;
		}
		untyped __cs__("System.Console.WriteLine({0})", str);
	}

	private static var _random:Dynamic = null;

	public static function random():Float {
		if (_random == null) {
			_random = untyped __cs__("new System.Random()");
		}
		return untyped __cs__("((System.Random){0}).NextDouble()", _random);
	}
}
