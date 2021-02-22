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

import python.Boot;
import python.Syntax;
import python.internal.Internal;
import python.internal.UBuiltins;
import python.lib.Inspect;

@:keepInit
@:coreApi class Std {
	@:access(python.Boot)
	public static function downcast<T:{}, S:T>(value:T, c:Class<S>):Null<S> {
		try {@:nullSafety(Off)
			return UBuiltins.isinstance(value, c) || (Inspect.isInterface(c) && Boot.implementsInterface(value, c)) ? cast value : null;
		} catch (e:Dynamic) {
			return null;
		}
	}

	@:deprecated('Std.instance() is deprecated. Use Std.downcast() instead.')
	public static inline function instance<T:{}, S:T>(value:T, c:Class<S>):Null<S> {
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

		if (Inspect.isclass(t)) {@:nullSafety(Off)
			return Boot.implementsInterface(v, t);
		} else {
			return false;
		}
	}

	@:access(python.Boot)
	public static function string(s:Null<Dynamic>):String {@:nullSafety(Off)
		return python.Boot.toString(s);
	}

	public static inline function int(x:Float):Int {
		try {
			return UBuiltins.int(x);
		} catch (e:Dynamic) {@:nullSafety(Off)
			return null;
		}
	}

	public static function parseInt(x:String):Null<Int> {
		if (x == null)
			return null;
		try {
			return UBuiltins.int(x);
		} catch (e:Dynamic) {
			var base = 10;
			var len = x.length;
			var foundCount = 0;
			var sign = 0;
			var firstDigitIndex = 0;
			var lastDigitIndex = -1;
			var previous = 0;

			for (i in 0...len) {
				var c = StringTools.fastCodeAt(x, i);
				switch c {
					case _ if ((c > 8 && c < 14) || c == 32):
						if (foundCount > 0) {
							return null;
						}
						continue;
					case '-'.code if (foundCount == 0):
						sign = -1;
					case '+'.code if (foundCount == 0):
						sign = 1;
					case '0'.code if (foundCount == 0 || (foundCount == 1 && sign != 0)):
					case 'x'.code | 'X'.code if (previous == '0'.code
						&& ((foundCount == 1 && sign == 0) || (foundCount == 2 && sign != 0))):
						base = 16;
					case _ if ('0'.code <= c && c <= '9'.code):
					case _ if (base == 16 && (('a'.code <= c && c <= 'z'.code) || ('A'.code <= c && c <= 'Z'.code))):
					case _:
						break;
				}
				if ((foundCount == 0 && sign == 0) || (foundCount == 1 && sign != 0)) {
					firstDigitIndex = i;
				}
				foundCount++;
				lastDigitIndex = i;
				previous = c;
			}
			if (firstDigitIndex <= lastDigitIndex) {
				var digits = x.substring(firstDigitIndex, lastDigitIndex + 1);
				return try {
					(sign == -1 ? -1 : 1) * UBuiltins.int(digits, base);
				} catch (e:Dynamic) {
					null;
				}
			}
			return null;
		}
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
