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

@:coreApi
class Std {
	@:deprecated('Std.is is deprecated. Use Std.isOfType instead.')
	public static inline function is(v:Dynamic, t:Dynamic):Bool {
		return isOfType(v, t);
	}

	public static function isOfType(v:Dynamic, t:Dynamic):Bool {
		if (v == null) {
			return false;
		}
		// t should be a System.Type (from typeof())
		// Use C# reflection to check if v is an instance of that type
		return untyped __cs__("{0} is System.Type typeObj && typeObj.IsInstanceOfType({1})", t, v);
	}

	public static function string(s:Dynamic):String {
		if (s == null) {
			return "null";
		}
		return cs.Cs.toString(s);
	}

	public static function int(x:Float):Int {
		return cast x;
	}

	public static function parseInt(x:String):Null<Int> {
		if (x == null)
			return null;

		var len = x.length;
		var index = 0;

		// skip whitespace
		while (index < len) {
			var c = StringTools.fastCodeAt(x, index);
			if (!((c > 8 && c < 14) || c == 32))
				break;
			++index;
		}

		// handle sign
		var isNegative = false;
		if (index < len) {
			var sign = StringTools.fastCodeAt(x, index);
			if (sign == '-'.code || sign == '+'.code) {
				isNegative = sign == '-'.code;
				++index;
			}
		}

		// handle base
		var isHexadecimal = false;
		if (index + 1 < len) {
			var cur = StringTools.fastCodeAt(x, index);
			var next = StringTools.fastCodeAt(x, index + 1);
			if (cur == '0'.code && (next == 'x'.code || next == 'X'.code)) {
				isHexadecimal = true;
				index += 2;
			}
		}

		// handle digits
		var firstInvalidIndex = index;
		while (firstInvalidIndex < len) {
			var c = StringTools.fastCodeAt(x, firstInvalidIndex);
			if (isHexadecimal) {
				if (!(('0'.code <= c && c <= '9'.code) || ('a'.code <= c && c <= 'f'.code) || ('A'.code <= c && c <= 'F'.code)))
					break;
			} else {
				if (!('0'.code <= c && c <= '9'.code))
					break;
			}
			++firstInvalidIndex;
		}

		// no valid digits
		if (index == firstInvalidIndex)
			return null;

		var result = cs.Cs.parseInt(x.substring(index, firstInvalidIndex), isHexadecimal ? 16 : 10);
		return isNegative ? -result : result;
	}

	public static function parseFloat(x:String):Float {
		if (x == null) {
			return Math.NaN;
		}
		x = StringTools.ltrim(x);
		if (x.length == 0) {
			return Math.NaN;
		}
		return cs.Cs.parseFloat(x);
	}

	inline public static function downcast<T:{}, S:T>(value:T, c:Class<S>):Null<S> {
		return Std.isOfType(value, c) ? cast value : null;
	}

	@:deprecated('Std.instance() is deprecated. Use Std.downcast() instead.')
	inline public static function instance<T:{}, S:T>(value:T, c:Class<S>):Null<S> {
		return downcast(value, c);
	}

	public static function random(x:Int):Int {
		if (x <= 0) {
			return 0;
		}
		return Std.int(Math.random() * x);
	}
}
