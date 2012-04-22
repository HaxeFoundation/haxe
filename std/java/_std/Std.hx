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
import java.Boot;
import java.Lib;
import haxe.lang.Exceptions;
 
@:core_api @:nativegen class Std {
	public static function is( v : Dynamic, t : Dynamic ) : Bool 
	{
		if (v == null) 
			return v == t;
		var clt:Class<Dynamic> = cast t;
		if (clt == null)
			return false;
		
		if (t == Float)
		{
			return untyped __java__('haxe.lang.Runtime.isDouble(v)');
		} else if (t == Int) {
			return untyped __java__('haxe.lang.Runtime.isInt(v)');
		} else if (t == Bool) {
			return untyped __java__('v instanceof java.lang.Boolean');
		}
			
		var native:java.lang.Class<Dynamic> = untyped clt.nativeType();
		
		return native.isAssignableFrom(Lib.getNativeType(v));
	}

	public static inline function string( s : Dynamic ) : String {
		return s + "";
	}

	public static inline function int( x : Float ) : Int {
		return cast x;
	}
	
	@:functionBody('
		if (x == null) return null;
		
		x = x.trim();
		int ret = 0;
		int base = 10;
		
		if (x.startsWith("0x"))
		{
			x = x.substring(2);
			base = 16;
		}
		
		int len = x.length();
		boolean foundAny = false;
		boolean isNeg = false;
		boolean hasValue = false;
		for (int i = 0; i < len; i++)
		{
			char c = x.charAt(i);
			if (!foundAny && c == \'-\') {
				isNeg = true;
				continue;
			}
			
			if (c >= \'0\' && c <= \'9\')
			{
				if (!foundAny && c == \'0\')
				{
					hasValue = true;
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
		
		if (foundAny || hasValue)
			return isNeg ? -ret : ret;
		else
			return null;
	')
	public static function parseInt( x : String ) : Null<Int> {
		return null;
	}
	
	@:functionBody('
		if (x == null) return java.lang.Double.NaN;
		
		x = x.trim();
		double ret = 0.0;
		double div = 0.0;
		double e = 0.0;
		
		int len = x.length();
		boolean hasValue = false;
		boolean foundAny = false;
		boolean isNeg = false;
		for (int i = 0; i < len; i++)
		{
			char c = x.charAt(i);
			if (!foundAny && c == \'-\') {
				isNeg = true;
				continue;
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
					hasValue = true;
					continue;
				}
				ret *= 10.0; foundAny = true; div *= 10.0;
				
				ret += ((int) (c - \'0\'));
			} else if (foundAny && c == \'E\' || c == \'e\') {
				boolean eNeg = false;
				if (i + 1 < len && x.charAt(i + 1) == \'-\')
				{
					eNeg = true;
					i++;
				}
				
				while (++i < len)
				{
					c = x.charAt(i);
					if (c >= \'0\' && c <= \'9\')
					{
						if (!foundAny && c == \'0\')
							continue;
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
		
		if (foundAny || hasValue)
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
		return Std.int(Math.random() * x);
	}

	@:macro public static function format( fmt : haxe.macro.Expr.ExprRequire<String> ) : haxe.macro.Expr.ExprRequire<String> {
		return haxe.macro.Format.format(fmt);
	}

}
	