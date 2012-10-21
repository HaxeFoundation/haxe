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
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT
 * , STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package haxe;

#if !haxe3

@:nativegen
class Int32
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

	public static function ucompare( a : Int32, b : Int32 ) : Int
	{
		if( isNeg(a) )
			return isNeg(b) ? compare(complement(b),complement(a)) : 1;
		return isNeg(b) ? -1 : compare(a,b);
	}

	public static inline function toNativeInt(a:Int32) : Int
	{
		return cast a;
	}
}

#end
