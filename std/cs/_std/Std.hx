/*
 * Copyright (C)2005-2012 Haxe Foundation
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

@:coreApi @:nativegen class Std {
	public static function is( v : Dynamic, t : Dynamic ) : Bool
	{
		if (v == null)
			return t == Dynamic;
		if (t == null)
			return false;
		var clt:Class<Dynamic> = cast t;
		if (clt == null)
			return false;
		var name:String = cast clt;

		switch(name)
		{
			case "System.Double":
				return untyped __cs__('v is double || v is int');
			case "System.Int32":
				return untyped __cs__('haxe.lang.Runtime.isInt(v)');
			case "System.Boolean":
				return untyped __cs__('v is bool');
			case "System.Object":
				return true;
		}

		var clv:Class<Dynamic> = untyped __cs__('v.GetType()');

		return untyped clt.IsAssignableFrom(clv);
	}

	public static function string( s : Dynamic ) : String {
		if (s == null)
			return "null";
		if (Std.is(s, Bool))
			return cast(s, Bool) ? "true" : "false";

		return s.ToString();
	}

	public static inline function int( x : Float ) : Int {
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

		var foundAny = false;
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
					case ' '.code, '\t'.code, '\n'.code, '\r'.code:
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

		var ret = 0.0;
		var div = 0.0;
		var e = 0.0;

		var len = x.length;
		var foundAny = false;
		var isNeg = false;
		var i = -1;
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
					case ' '.code, '\t'.code, '\n'.code, '\r'.code:
						if (isNeg)
							return Math.NaN;
						continue;
				}
			}

			if (c == '.'.code)
			{
				if (div != 0.0)
					break;
				div = 1.0;

				continue;
			}

			if (c >= '0'.code && c <= '9'.code)
			{
				if (!foundAny && c == '0'.code)
				{
					foundAny = true;
					continue;
				}

				ret *= 10; foundAny = true; div *= 10;

				ret += c - '0'.code;
			} else if (foundAny && (c == 'e'.code || c == 'E'.code)) {
				var eNeg = false;
				var eFoundAny = false;
				if (i + 1 < len)
				{
					var next = untyped cast(x[i + 1], Int);
					if (next == '-'.code)
					{
						eNeg = true;
						i++;
					} else if (next == '+'.code) {
						i++;
					}
				}

				while (++i < len)
				{
					c = untyped cast(x[i], Int);
					if (c >= '0'.code && c <= '9'.code)
					{
						if (!eFoundAny && c == '0'.code)
							continue;
						eFoundAny = true;
						e *= 10;
						e += c - '0'.code;
					} else {
						break;
					}
				}

				if (eNeg) e = -e;
			} else {
				break;
			}
		}

		if (div == 0.0) div = 1.0;

		if (foundAny)
		{
			ret = isNeg ? -(ret / div) : (ret / div);
			if (e != 0.0)
			{
				return ret * Math.pow(10.0, e);
			} else {
				return ret;
			}
		} else {
			return Math.NaN;
		}
	}

	public static function random( x : Int ) : Int {
		if (x <= 0) return 0;
		return untyped Math.rand.Next(x);
	}

	#if !haxe3
	@:macro public static function format( fmt : haxe.macro.Expr.ExprOf<String> ) : haxe.macro.Expr.ExprRequire<String> {
		return haxe.macro.Format.format(fmt);
	}
	#end

}
