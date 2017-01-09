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
import cs.Boot;
import cs.Lib;
import cs.internal.Exceptions;

@:coreApi @:nativeGen class Std {
	public static function is( v : Dynamic, t : Dynamic ) : Bool
	{
		if (v == null)
			return t == Dynamic;
		if (t == null)
			return false;
		var clt = cs.Lib.as(t, cs.system.Type);
		if (clt == null)
			return false;

		switch(clt.ToString())
		{
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

	public static function string( s : Dynamic ) : String {
		if (s == null)
			return "null";
		if (Std.is(s, Bool))
			return cast(s, Bool) ? "true" : "false";

		return s.ToString();
	}

	public static function int( x : Float ) : Int {
		return cast x;
	}

	public static function parseInt( x : String ) : Null<Int> {
		if (x == null) return null;

		var ret = 0;
		var base = 10;
		var i = -1;
		var len = x.length;

		if (StringTools.startsWith(x, "0") && len > 2)
		{
			var c:Int = cast untyped x[1];
			if (c == 'x'.code || c == 'X'.code)
			{
				i = 1;
				base = 16;
			}
		}

		var foundAny = i != -1;
		var isNeg = false;
		while (++i < len)
		{
			var c = cast(untyped x[i], Int); //fastCodeAt
			if (!foundAny)
			{
				switch(c)
				{
					case '-'.code:
						isNeg = true;
						continue;
					case ' '.code, '\t'.code, '\n'.code, '\r'.code, '+'.code:
						if (isNeg)
							return null;
						continue;
				}
			}

			if (c >= '0'.code && c <= '9'.code)
			{
				if (!foundAny && c == '0'.code)
				{
					foundAny = true;
					continue;
				}
				ret *= base; foundAny = true;

				ret += c - '0'.code;
			} else if (base == 16) {
				if (c >= 'a'.code && c <= 'f'.code) {
					ret *= base; foundAny = true;
					ret += c - 'a'.code + 10;
				} else if (c >= 'A'.code && c <= 'F'.code) {
					ret *= base; foundAny = true;
					ret += c - 'A'.code + 10;
				} else {
					break;
				}
			} else {
				break;
			}
		}

		if (foundAny)
			return isNeg ? -ret : ret;
		else
			return null;
	}

	public static function parseFloat( x : String ) : Float {
		if (x == null) return Math.NaN;
		x = StringTools.ltrim(x);
		var found = false, hasDot = false, hasSign = false,
		    hasE = false, hasESign = false, hasEData = false;
		var i = -1;
		inline function getch(i:Int):Int return cast ((untyped x : cs.system.String)[i]);

		while (++i < x.length)
		{
			var chr = getch(i);
			if (chr >= '0'.code && chr <= '9'.code)
			{
				if (hasE)
				{
					hasEData = true;
				}
				found = true;
			} else switch (chr) {
				case 'e'.code | 'E'.code if(!hasE):
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
		if (hasE && !hasEData)
		{
			i--;
			if (hasESign)
				i--;
		}
		if (i != x.length)
		{
			x = x.substr(0,i);
		}
		return try
			cs.system.Double.Parse(x, cs.system.globalization.CultureInfo.InvariantCulture)
		catch(e:Dynamic)
			Math.NaN;
	}

	@:extern inline public static function instance<T:{},S:T>( value : T, c : Class<S> ) : S {
		return cs.Lib.as(value,c);
	}

	public static function random( x : Int ) : Int {
		if (x <= 0) return 0;
		return untyped Math.rand.Next(x);
	}
}
