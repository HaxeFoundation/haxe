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

import cs.Boot;
import cs.Lib;

@:coreApi @:nativeGen class Std {
	public static inline function is(v:Dynamic, t:Dynamic):Bool {
		return isOfType(v, t);
	}

	public static function isOfType(v:Dynamic, t:Dynamic):Bool {
		if (v == null)
			return false;
		if (t == null)
			return false;
		var clt = cs.Lib.as(t, cs.system.Type);
		if (clt == null)
			return false;

		switch (clt.ToString()) {
			case "System.Double":
				return untyped __cs__('{0} is double || {0} is int', v);
			case "System.Int32":
				return cs.internal.Runtime.isInt(v);
			case "System.Boolean":
				return untyped __cs__('{0} is bool', v);
			case "System.Object":
				return true;
		}

		var vt = cs.Lib.getNativeType(v);

		if (clt.IsAssignableFrom(vt))
			return true;

		#if !erase_generics
		for (iface in clt.GetInterfaces()) {
			var g = cs.internal.Runtime.getGenericAttr(iface);
			if (g != null && g.generic == clt) {
				return iface.IsAssignableFrom(vt);
			}
		}
		#end

		return false;
	}

	public static function string(s:Dynamic):String {
		if (s == null)
			return "null";
		if (Std.isOfType(s, Bool))
			return cast(s, Bool) ? "true" : "false";

		return s.ToString();
	}

	public static function int(x:Float):Int {
		return cast x;
	}

	public static function parseInt(x:String):Null<Int> {
		if (x == null)
			return null;

		var base = 10;
		var len = x.length;
		var foundCount = 0;
		var sign = 0;
		var firstDigitIndex = 0;
		var lastDigitIndex = -1;
		var previous = 0;

		for(i in 0...len) {
			var c = StringTools.fastCodeAt(x, i);
			switch c {
				case _ if((c > 8 && c < 14) || c == 32):
					if(foundCount > 0) {
						return null;
					}
					continue;
				case '-'.code if(foundCount == 0):
					sign = -1;
				case '+'.code if(foundCount == 0):
					sign = 1;
				case '0'.code if(foundCount == 0 || (foundCount == 1 && sign != 0)):
				case 'x'.code | 'X'.code if(previous == '0'.code && ((foundCount == 1 && sign == 0) || (foundCount == 2 && sign != 0))):
					base = 16;
				case _ if('0'.code <= c && c <= '9'.code):
				case _ if(base == 16 && (('a'.code <= c && c <= 'z'.code) || ('A'.code <= c && c <= 'Z'.code))):
				case _:
					break;
			}
			if((foundCount == 0 && sign == 0) || (foundCount == 1 && sign != 0)) {
				firstDigitIndex = i;
			}
			foundCount++;
			lastDigitIndex = i;
			previous = c;
		}
		if(firstDigitIndex <= lastDigitIndex) {
			var digits = x.substring(firstDigitIndex, lastDigitIndex + 1);
			return try {
				(sign == -1 ? -1 : 1) * cs.system.Convert.ToInt32(digits, base);
			} catch(e:cs.system.FormatException) {
				null;
			}
		}
		return null;
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
			return cast((untyped x : cs.system.String)[i]);

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
		return try cs.system.Double.Parse(x, cs.system.globalization.CultureInfo.InvariantCulture) catch (e:Dynamic) Math.NaN;
	}

	extern inline public static function downcast<T:{}, S:T>(value:T, c:Class<S>):S {
		return cs.Lib.as(value, c);
	}

	@:deprecated('Std.instance() is deprecated. Use Std.downcast() instead.')
	extern inline public static function instance<T:{}, S:T>(value:T, c:Class<S>):S {
		return downcast(value, c);
	}

	public static function random(x:Int):Int {
		if (x <= 0)
			return 0;
		return untyped Math.rand.Next(x);
	}
}
