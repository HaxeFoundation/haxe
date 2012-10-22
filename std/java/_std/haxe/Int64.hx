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
using haxe.Int64;
private typedef NativeInt64 = Int;

@:coreApi
@:nativegen class Int64
{
	@:extern private static inline function asNative(i:Int64):NativeInt64 return untyped i
	@:extern private static inline function ofNative(i:NativeInt64):Int64 return untyped i
	@:extern private static inline function mkNative(i:Dynamic):NativeInt64 return i

	#if haxe3

	public static inline function make( high : Int, low : Int ) : Int64
	{
		return ((cast(high, NativeInt64) << 32 ) | (cast(low, NativeInt64))).ofNative();
	}

	public static inline function getLow( x : Int64 ) : Int
	{
		return cast (x.asNative() & 0xFFFFFFFF.mkNative());
	}

	public static inline function getHigh( x : Int64 ) : Int
	{
		return cast(x,NativeInt64) >>> 32;
	}

	#else

	public static inline function make( high : Int32, low : Int32 ) : Int64
	{
		return ((cast(high, NativeInt64) << 32 ) | (cast(low, NativeInt64))).ofNative();
	}

	public static inline function ofInt32( x : Int32 ) : Int64 {
		return cast x;
	}

	public static inline function getLow( x : Int64 ) : Int32
	{
		return cast (x.asNative() & 0xFFFFFFFF.mkNative());
	}

	public static inline function getHigh( x : Int64 ) : Int32
	{
		return cast(cast(x,NativeInt64) >>> 32, Int32);
	}

	#end

	public static inline function ofInt( x : Int ) : Int64 {
		return cast x;
	}

	public static inline function toInt( x : Int64 ) : Int
	{
		return cast x;
	}

	public static inline function add( a : Int64, b : Int64 ) : Int64
	{
		return (a.asNative() + b.asNative()).ofNative();
	}

	public static inline function sub( a : Int64, b : Int64 ) : Int64
	{
		return (a.asNative() - b.asNative()).ofNative();
	}

	public static inline function mul( a : Int64, b : Int64 ) : Int64 {
		return (a.asNative() * b.asNative()).ofNative();
	}

	static function divMod( modulus : Int64, divisor : Int64 ) : { quotient : Int64, modulus : Int64 }
	{
		var q:Int64 = (modulus.asNative() / divisor.asNative()).mkNative().ofNative();
		var m:Int64 = (modulus.asNative() % divisor.asNative()).mkNative().ofNative();
		return { quotient : q, modulus : m };
	}

	public static inline function div( a : Int64, b : Int64 ) : Int64 {
		return (a.asNative() / b.asNative()).mkNative().ofNative();
	}

	public static inline function mod( a : Int64, b : Int64 ) : Int64 {
		return (a.asNative() % b.asNative()).mkNative().ofNative();
	}

	public static inline function shl( a : Int64, b : Int ) : Int64 {
		return (a.asNative() << b).ofNative();
	}

	public static inline function shr( a : Int64, b : Int ) : Int64 {
		return (a.asNative() >> b).ofNative();
	}

	public static inline function ushr( a : Int64, b : Int ) : Int64 {
		return (a.asNative() >>> b).ofNative();
	}

	public static inline function and( a : Int64, b : Int64 ) : Int64
	{
		return (a.asNative() & b.asNative()).ofNative();
	}

	public static inline function or( a : Int64, b : Int64 ) : Int64
	{
		return (a.asNative() | b.asNative()).ofNative();
	}

	public static inline function xor( a : Int64, b : Int64 ) : Int64
	{
		return (a.asNative() ^ b.asNative()).ofNative();
	}

	public static inline function neg( a : Int64 ) : Int64
	{
		return (~a.asNative()).ofNative();
	}

	public static inline function isNeg( a : Int64 ) : Bool
	{
		return (a.asNative() < 0.mkNative());
	}

	public static inline function isZero( a : Int64 ) : Bool
	{
		return (a.asNative() == 0.mkNative());
	}

	public static inline function compare( a : Int64, b : Int64 ) : Int
	{
		return cast (a.asNative() - b.asNative());
	}

	/**
		Compare two Int64 in unsigned mode.
	**/
	public static function ucompare( a : Int64, b : Int64 ) : Int
	{
		if (a.asNative() < 0.mkNative())
			return (b.asNative() < 0.mkNative()) ? compare( (~a.asNative()).ofNative(), (~b.asNative()).ofNative()) : 1;
		return (b.asNative() < 0.mkNative()) ? -1 : compare(a, b);
	}

	public static inline function toStr( a : Int64 ) : String {
		return a + "";
	}
}


