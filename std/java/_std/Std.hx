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
import java.Boot;
import java.Lib;
import java.internal.Exceptions;

@:coreApi @:nativeGen class Std {
	public static function is( v : Dynamic, t : Dynamic ) : Bool
	{
		if (v == null)
			return t == Dynamic;
		if (t == null)
			return false;
		var clt:Class<Dynamic> = cast t;
		if (clt == null)
			return false;
		var name:String = untyped __java__("clt.getName()");

		switch(name)
		{
			case "double", "java.lang.Double":
				return untyped __java__('haxe.lang.Runtime.isDouble(v)');
			case "int", "java.lang.Integer":
				return untyped __java__('haxe.lang.Runtime.isInt(v)');
			case "boolean", "java.lang.Boolean":
				return untyped __java__('v instanceof java.lang.Boolean');
			case "java.lang.Object":
				return true;
		}

		var clv:Class<Dynamic> = untyped __java__('v.getClass()');

		return untyped clt.isAssignableFrom(clv);
	}

	public static inline function string( s : Dynamic ) : String {
		return cast s;
	}

	public static inline function int( x : Float ) : Int {
		return cast x;
	}

	@:functionCode('
		if (x == null) return null;

		int ret = 0;
		int base = 10;
		int i = 0;
		int len = x.length();

		if (x.startsWith("0") && len > 2)
		{
			char c = x.charAt(1);
			if (c == \'x\' || c == \'X\')
			{
				i = 2;
				base = 16;
			}
		}

		boolean foundAny = false;
		boolean isNeg = false;
		for (; i < len; i++)
		{
			char c = x.charAt(i);
			if (!foundAny)
			{
				switch(c)
				{
					case \'-\':
						isNeg = true;
						continue;
					case \'\\n\':
					case \'\\t\':
					case \'\\r\':
					case \' \':
						if (isNeg) return null;
						continue;
				}
			}

			if (c >= \'0\' && c <= \'9\')
			{
				if (!foundAny && c == \'0\')
				{
					foundAny = true;
					continue;
				}
				ret *= base; foundAny = true;

				ret += ((int) (c - \'0\'));
			} else if (base == 16) {
				if (c >= \'a\' && c <= \'f\') {
					ret *= base; foundAny = true;
					ret += ((int) (c - \'a\')) + 10;
				} else if (c >= \'A\' && c <= \'F\') {
					ret *= base; foundAny = true;
					ret += ((int) (c - \'A\')) + 10;
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
	')
	public static function parseInt( x : String ) : Null<Int> {
		return null;
	}

	@:functionCode('
		if (x == null) return java.lang.Double.NaN;

		x = x.trim();
		double ret = 0.0;
		double div = 0.0;
		double e = 0.0;

		int len = x.length();
		boolean foundAny = false;
		boolean isNeg = false;
		for (int i = 0; i < len; i++)
		{
			char c = x.charAt(i);
			if (!foundAny)
			{
				switch(c)
				{
					case \'-\':
						isNeg = true;
						continue;
					case \'\\n\':
					case \'\\t\':
					case \'\\r\':
					case \' \':
					if (isNeg) return java.lang.Double.NaN;
						continue;
				}
			}

			if (c == \'.\') {
				if (div != 0.0)
					break;
				div = 1.0;

				continue;
			}

			if (c >= \'0\' && c <= \'9\')
			{
				if (!foundAny && c == \'0\')
				{
					foundAny = true;
					continue;
				}
				ret *= 10.0; foundAny = true; div *= 10.0;

				ret += ((int) (c - \'0\'));
			} else if (foundAny && c == \'E\' || c == \'e\') {
				boolean eNeg = false;
				boolean eFoundAny = false;

				char next = x.charAt(i + 1);
				if (i + 1 < len)
				{
					if (next == \'-\')
					{
						eNeg = true;
						i++;
					} else if (next == \'+\') {
						i++;
					}
				}

				while (++i < len)
				{
					c = x.charAt(i);
					if (c >= \'0\' && c <= \'9\')
					{
						if (!eFoundAny && c == \'0\')
							continue;
						eFoundAny = true;
						e *= 10.0;
						e += ((int) (c - \'0\'));
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
			return java.lang.Double.NaN;
		}
	')
	public static function parseFloat( x : String ) : Float {
		return 0.0;
	}

	public static function random( x : Int ) : Int {
		if (x <= 0) return 0;
		return Std.int(Math.random() * x);
	}

}
