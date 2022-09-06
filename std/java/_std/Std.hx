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

import java.Boot;
import java.Lib;

using StringTools;

@:coreApi @:nativeGen class Std {
	@:deprecated('Std.is is deprecated. Use Std.isOfType instead.')
	public static inline function is(v:Dynamic, t:Dynamic):Bool {
		return isOfType(v, t);
	}

	public static function isOfType(v:Dynamic, t:Dynamic):Bool {
		if (v == null)
			return false;
		if (t == null)
			return false;
		var clt:java.lang.Class<Dynamic> = cast t;
		if (clt == null)
			return false;
		var name:String = clt.getName();

		switch (name) {
			case "double", "java.lang.Double":
				return untyped __java__('haxe.lang.Runtime.isDouble(v)');
			case "int", "java.lang.Integer":
				return untyped __java__('haxe.lang.Runtime.isInt(v)');
			case "boolean", "java.lang.Boolean":
				return untyped __java__('v instanceof java.lang.Boolean');
			case "java.lang.Object":
				return true;
		}

		var clv:java.lang.Class<Dynamic> = untyped __java__('v.getClass()');

		return clt.isAssignableFrom(clv);
	}

	public static function string(s:Dynamic):String {
		return cast(s, String) + "";
	}

	public static function int(x:Float):Int {
		return cast x;
	}

	static inline function isSpaceChar(code:Int):Bool
		return (code > 8 && code < 14) || code == 32;

	static inline function isHexPrefix(cur:Int, next:Int):Bool
		return cur == '0'.code && (next == 'x'.code || next == 'X'.code);

	static inline function isDecimalDigit(code:Int):Bool
		return '0'.code <= code && code <= '9'.code;

	static inline function isHexadecimalDigit(code:Int):Bool
		return isDecimalDigit(code) || ('a'.code <= code && code <= 'f'.code) || ('A'.code <= code && code <= 'F'.code);

	public static function parseInt(x:String):Null<Int> {
		if (x == null)
			return null;

		final len = x.length;
		var index = 0;

		inline function hasIndex(index:Int)
			return index < len;

		// skip whitespace
		while (hasIndex(index)) {
			if (!isSpaceChar(x.unsafeCodeAt(index)))
				break;
			++index;
		}

		// handle sign
		final isNegative = hasIndex(index) && {
			final sign = x.unsafeCodeAt(index);
			if (sign == '-'.code || sign == '+'.code) {
				++index;
			}
			sign == '-'.code;
		}

		// handle base
		final isHexadecimal = hasIndex(index + 1) && isHexPrefix(x.unsafeCodeAt(index), x.unsafeCodeAt(index + 1));
		if (isHexadecimal)
			index += 2; // skip prefix

		// handle digits
		final firstInvalidIndex = {
			var cur = index;
			if (isHexadecimal) {
				while (hasIndex(cur)) {
					if (!isHexadecimalDigit(x.unsafeCodeAt(cur)))
						break;
					++cur;
				}
			} else {
				while (hasIndex(cur)) {
					if (!isDecimalDigit(x.unsafeCodeAt(cur)))
						break;
					++cur;
				}
			}
			cur;
		}

		// no valid digits
		if (index == firstInvalidIndex)
			return null;

		final result = java.lang.Integer.parseInt(x.substring(index, firstInvalidIndex), if (isHexadecimal) 16 else 10);
		return if (isNegative) -result else result;
	}

	public static function parseFloat(x:String):Float {
		if (x == null)
			return Math.NaN;
		x = StringTools.ltrim(x);
		var found = false,
			hasDot = false,
			hasSign = false,
			hasE = false,
			hasESign = false,
			hasEData = false;
		var i = -1;
		inline function getch(i:Int):Int
			return cast(untyped x._charAt(i) : java.StdTypes.Char16);

		while (++i < x.length) {
			var chr = getch(i);
			if (chr >= '0'.code && chr <= '9'.code) {
				if (hasE) {
					hasEData = true;
				}
				found = true;
			} else
				switch (chr) {
					case 'e'.code | 'E'.code if (!hasE):
						hasE = true;
					case '.'.code if (!hasDot):
						hasDot = true;
					case '-'.code, '+'.code if (!found && !hasSign):
						hasSign = true;
					case '-'.code | '+'.code if (found && !hasESign && hasE && !hasEData):
						hasESign = true;
					case _:
						break;
				}
		}
		if (hasE && !hasEData) {
			i--;
			if (hasESign)
				i--;
		}

		if (i != x.length) {
			x = x.substr(0, i);
		}
		return try java.lang.Double.DoubleClass.parseDouble(x) catch (e:Dynamic) Math.NaN;
	}

	inline public static function downcast<T:{}, S:T>(value:T, c:Class<S>):S {
		return Std.isOfType(value, c) ? cast value : null;
	}

	@:deprecated('Std.instance() is deprecated. Use Std.downcast() instead.')
	inline public static function instance<T:{}, S:T>(value:T, c:Class<S>):S {
		return downcast(value, c);
	}

	public static function random(x:Int):Int {
		if (x <= 0)
			return 0;
		return Std.int(Math.random() * x);
	}
}
