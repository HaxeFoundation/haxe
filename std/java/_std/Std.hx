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
		var clt:java.lang.Class<Dynamic> = cast t;
		if (clt == null)
			return false;
		var name:String = clt.getName();

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

		var clv:java.lang.Class<Dynamic> = untyped __java__('v.getClass()');

		return clt.isAssignableFrom(clv);
	}

	public static function string( s : Dynamic ) : String {
		return cast(s, String) + "";
	}

	public static function int( x : Float ) : Int {
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

		boolean foundAny = i != 0;
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
					case \'+\':
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

	public static function parseFloat( x : String ) : Float {
		if (x == null) return Math.NaN;
		x = StringTools.ltrim(x);
		var found = false, hasDot = false, hasSign = false,
		    hasE = false, hasESign = false, hasEData = false;
		var i = -1;
		inline function getch(i:Int):Int return cast (untyped x._charAt(i) : java.StdTypes.Char16);

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
			java.lang.Double.DoubleClass.parseDouble(x)
		catch(e:Dynamic)
			Math.NaN;
	}

	inline public static function instance<T:{},S:T>( value : T, c : Class<S> ) : S {
		return Std.is(value, c) ? cast value : null;
	}

	public static function random( x : Int ) : Int {
		if (x <= 0) return 0;
		return Std.int(Math.random() * x);
	}

}
