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
package haxe;

private typedef NativeInt64 = Int64;

@:nativegen class Int64 
{
	public static inline function make( high : Int32, low : Int32 ) : Int64 
	{
		return (cast(high, NativeInt64) << 32 ) | (cast(low, NativeInt64));
	}

	public static inline function ofInt( x : Int ) : Int64 {
		return cast x;
	}

	public static inline function ofInt32( x : Int32 ) : Int64 {
		return cast x;
	}

	public static inline function toInt( x : Int64 ) : Int 
	{
		return cast x;
	}

	public static inline function getLow( x : Int64 ) : Int32 
	{
		return cast x;
	}

	public static inline function getHigh( x : Int64 ) : Int32 
	{
		return cast (cast(x,NativeInt64) >>> 32, Int32);
	}

	public static inline function add( a : Int64, b : Int64 ) : Int64 
	{
		return cast(a, NativeInt64) + cast(b, NativeInt64);
	}

	public static inline function sub( a : Int64, b : Int64 ) : Int64 
	{
		return cast(a, NativeInt64) - cast(b, NativeInt64);
	}

	public static inline function mul( a : Int64, b : Int64 ) : Int64 {
		return cast(a, NativeInt64) * cast(b, NativeInt64);
	}

	static function divMod( modulus : Int64, divisor : Int64 ) 
	{
		var q:NativeInt64 = cast (cast(modulus, NativeInt64) / cast(divisor, NativeInt64));
		var m:NativeInt64 = cast(modulus, NativeInt64) % cast(divisor, NativeInt64);
		return { quotient : q, modulus : m };
	}

	public static inline function div( a : Int64, b : Int64 ) : Int64 {
		return cast (cast(a, NativeInt64) / cast(b, NativeInt64));
	}

	public static inline function mod( a : Int64, b : Int64 ) : Int64 {
		return cast(a, NativeInt64) % cast(b, NativeInt64);
	}

	public static inline function shl( a : Int64, b : Int ) : Int64 {
		return cast(a, NativeInt64) << cast(b, NativeInt64);
	}

	public static inline function shr( a : Int64, b : Int ) : Int64 {
		return cast(a, NativeInt64) >> cast(b, NativeInt64);
	}

	public static inline function ushr( a : Int64, b : Int ) : Int64 {
		return cast(a, NativeInt64) >>> b;
	}

	public static inline function and( a : Int64, b : Int64 ) : Int64 
	{
		return cast(a, NativeInt64) & cast(b, NativeInt64);
	}

	public static inline function or( a : Int64, b : Int64 ) : Int64 
	{
		return cast(a, NativeInt64) | cast(b, NativeInt64);
	}

	public static inline function xor( a : Int64, b : Int64 ) : Int64 
	{
		return cast(a, NativeInt64) ^ cast(b, NativeInt64);
	}

	public static inline function neg( a : Int64 ) : Int64 
	{
		return -cast(a, NativeInt64);
	}

	public static inline function isNeg( a : Int64 ) : Bool 
	{
		return cast(a, NativeInt64) < cast(0, NativeInt64);
	}

	public static inline function isZero( a : Int64 ) : Bool 
	{
		return cast(a, NativeInt64) == cast(0, NativeInt64);
	}

	public static inline function compare( a : Int64, b : Int64 ) : Int 
	{
		return cast(cast(a, NativeInt64) - cast(b, NativeInt64), Int);
	}

	/**
		Compare two Int64 in unsigned mode.
	**/
	public static function ucompare( a : Int64, b : Int64 ) : Int 
	{
		var a:NativeInt64 = cast a;
		var b:NativeInt64 = cast b;
		if (a < cast(0, NativeInt64))
			return (b < cast(0, NativeInt64)) ? compare(~a, ~b) : 1;
		return (b < cast(0, NativeInt64)) ? -1 : compare(a, b);
	}

	public static inline function toStr( a : Int64 ) : String {
		return a + "";
	}
}


