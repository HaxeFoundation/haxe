/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
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

	@:macro public static function format( fmt : haxe.macro.Expr.ExprRequire<String> ) : haxe.macro.Expr.ExprRequire<String> {
		return haxe.macro.Format.format(fmt);
	}

}
