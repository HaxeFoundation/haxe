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
package haxe;

#if !haxe3

@:nativeGen class Int32
{
	public static inline function make( a : Int, b : Int ) : Int32
	{
		return cast ((a << 16) | b);
	}

	public static inline function ofInt( x : Int ) : Int32
	{
		return cast x;
	}

	public static function toInt( x : Int32 ) : Int
	{
		if ( (((cast x) >> 30) & 1) != ((cast x) >>> 31) ) throw "Overflow " + x;

		return cast x;
	}

	public static inline function add( a : Int32, b : Int32 ) : Int32
	{
		return cast ((cast a) + cast b);
	}

	public static inline function sub( a : Int32, b : Int32 ) : Int32
	{
		return cast ((cast a) - cast b);
	}

	public static inline function mul( a : Int32, b : Int32 ) : Int32
	{
		return cast ((cast a) * cast b);
	}

	public static inline function div( a : Int32, b : Int32 ) : Int32
	{
		return cast ((cast a) / cast b);
	}

	public static inline function mod( a : Int32, b : Int32 ) : Int32
	{
		return cast ((cast a) % cast b);
	}

	public static inline function shl( a : Int32, b : Int ) : Int32
	{
		return cast ((cast a) << b);
	}

	public static inline function shr( a : Int32, b : Int ) : Int32
	{
		return cast ((cast a) >> b);
	}

	public static inline function ushr( a : Int32, b : Int ) : Int32
	{
		return cast ((cast a) >>> b);
	}

	public static inline function and( a : Int32, b : Int32 ) : Int32
	{
		return cast ((cast a) & cast b);
	}

	public static inline function or( a : Int32, b : Int32 ) : Int32
	{
		return cast ((cast a) | cast b);
	}

	public static inline function xor( a : Int32, b : Int32 ) : Int32
	{
		return cast ((cast a) ^ cast b);
	}

	public static inline function neg( a : Int32 ) : Int32
	{
		return cast -(cast a);
	}

	public static inline function complement( a : Int32 ) : Int32
	{
		return cast ~(cast a);
	}

	public static inline function compare( a : Int32, b : Int32 ) : Int
	{
		return (cast a) - cast b;
	}

	public static inline function isNeg( a : Int32 ) : Bool
	{
		return (cast a) < 0;
	}

	public static inline function isZero( a : Int32 ) : Bool
	{
		return (cast a) == 0;
	}

	public static inline function ucompare( a : Int32, b : Int32 ) : Int
	{
		var ua:UInt = cast a;
		var ub:UInt = cast b;

		return (ua < ub) ? -1 : ( (ua > ub) ? 1 : 0 );
	}

	public static inline function toNativeInt(a:Int32) : Int
	{
		return cast a;
	}
}

#end
