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

package;

import python.internal.Internal;
import python.internal.UBuiltins;
import python.lib.Inspect;
import python.Boot;
import python.Syntax;

@:keepInit
@:coreApi class Std {
	@:access(python.Boot)
	public static function downcast<T:{}, S:T>(value:T, c:Class<S>):S {
		try {
			return UBuiltins.isinstance(value, c) || (Inspect.isInterface(c) && Boot.implementsInterface(value, c)) ? cast value : null;
		} catch (e:Dynamic) {
			return null;
		}
	}

	@:deprecated('Std.instance() is deprecated. Use Std.downcast() instead.')
	public static inline function instance<T:{}, S:T>(value:T, c:Class<S>):S {
		return downcast(value, c);
	}

	@:access(python.Boot)
	static inline function isMetaType(v:Dynamic, t:Dynamic):Bool {
		return Boot.isMetaType(v, t);
	}

	@:ifFeature("typed_cast")
	@:deprecated('Std.is is deprecated. Use Std.isOfType instead.')
	public static inline function is(v:Dynamic, t:Dynamic):Bool {
		return isOfType(v, t);
	}

	@:access(python.Boot)
	@:ifFeature("typed_cast")
	public static function isOfType(v:Dynamic, t:Dynamic):Bool {
		if (v == null && t == null) {
			return false;
		}
		if (t == null) {
			return false;
		}
		if (isMetaType(t, Dynamic)) {
			return v != null;
		}
		var isBool = UBuiltins.isinstance(v, UBuiltins.bool);

		if (isMetaType(t, Bool) && isBool) {
			return true;
		}
		if (!isBool && !isMetaType(t, Bool) && isMetaType(t, Int) && UBuiltins.isinstance(v, UBuiltins.int)) {
			return true;
		}
		var vIsFloat = UBuiltins.isinstance(v, UBuiltins.float);

		if (!isBool && vIsFloat && isMetaType(t, Int) && Math.isFinite(v) && v == Std.int(v) && v <= 2147483647 && v >= -2147483648) {
			return true;
		}

		if (!isBool && isMetaType(t, Float) && (UBuiltins.isinstance(v, python.Syntax.tuple(UBuiltins.float, UBuiltins.int)))) {
			return true;
		}

		if (isMetaType(t, UBuiltins.str)) {
			return UBuiltins.isinstance(v, String);
		}
		var isEnumType = isMetaType(t, Enum);
		if (isEnumType && Inspect.isclass(v) && Internal.hasConstructs(v))
			return true;

		if (isEnumType)
			return false;

		var isClassType = isMetaType(t, Class);
		if (isClassType
			&& !UBuiltins.isinstance(v, Enum)
			&& Inspect.isclass(v)
			&& Internal.hasClassName(v)
			&& !Internal.hasConstructs(v))
			return true;

		if (isClassType)
			return false;

		if (try UBuiltins.isinstance(v, t) catch (e:Dynamic) false) {
			return true;
		}

		if (Inspect.isclass(t)) {
			return Boot.implementsInterface(v, t);
		} else {
			return false;
		}
	}

	@:access(python.Boot)
	public static function string(s:Dynamic):String {
		return python.Boot.toString(s);
	}

	public static inline function int(x:Float):Int {
		try {
			return UBuiltins.int(x);
		} catch (e:Dynamic) {
			return null;
		}
	}

	static inline function isSpaceChar(char:String):Bool
		return Syntax.isIn(char, " \n\r\t\x0b\x0c");

	static inline function isHexPrefix(cur:String, next:String):Bool
		return cur == '0' && (next == 'x' || next == 'X');

	static inline function isDecimalDigit(char:String):Bool
		return Syntax.isIn(char, "0123456789");

	static inline function isHexadecimalDigit(char:String):Bool
		return Syntax.isIn(char, "0123456789abcdefABCDEF");

	public static function parseInt(x:String):Null<Int> {
		if (x == null)
			return null;

		final len = x.length;
		var index = 0;

		inline function hasIndex(index:Int)
			return index < len;

		// skip whitespace
		while (hasIndex(index)) {
			if (!isSpaceChar(Syntax.arrayAccess(x, index)))
				break;
			++index;
		}

		// handle sign
		final isNegative = hasIndex(index) && {
			final sign = Syntax.arrayAccess(x, index);
			if (sign == '-' || sign == '+') {
				++index;
			}
			sign == '-';
		}

		// handle base
		final isHexadecimal = hasIndex(index + 1) && isHexPrefix(Syntax.arrayAccess(x, index), Syntax.arrayAccess(x, index + 1));
		if (isHexadecimal)
			index += 2; // skip prefix

		// handle digits
		final firstInvalidIndex = {
			var cur = index;
			if (isHexadecimal) {
				while (hasIndex(cur)) {
					if (!isHexadecimalDigit(Syntax.arrayAccess(x, cur)))
						break;
					++cur;
				}
			} else {
				while (hasIndex(cur)) {
					if (!isDecimalDigit(Syntax.arrayAccess(x, cur)))
						break;
					++cur;
				}
			}
			cur;
		}

		// no valid digits
		if (index == firstInvalidIndex)
			return null;

		final result = python.internal.UBuiltins.int(x.substring(index, firstInvalidIndex), if (isHexadecimal) 16 else 10);
		return if (isNegative) -result else result;
	}

	static function shortenPossibleNumber(x:String):String {
		var r = "";
		for (i in 0...x.length) {
			var c = x.charAt(i);
			switch (c.charCodeAt(0)) {
				case "0".code | "1".code | "2".code | "3".code | "4".code | "5".code | "6".code | "7".code | "8".code | "9".code | ".".code:
					r += c;
				case _:
					break;
			}
		}
		return r;
	}

	public static function parseFloat(x:String):Float {
		try {
			return UBuiltins.float(x);
		} catch (e:Dynamic) {
			if (x != null) {
				var r1 = shortenPossibleNumber(x);
				if (r1 != x) {
					return parseFloat(r1);
				}
			}
			return Math.NaN;
		}
	}

	public static inline function random(x:Int):Int {
		return if (x <= 0) 0 else python.internal.UBuiltins.int(Math.random() * x);
	}
}
