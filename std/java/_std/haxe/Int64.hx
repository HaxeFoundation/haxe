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

@:nativegen class Int64
{
	@:extern private static inline function asNative(i:haxe.Int64):NativeInt64 return untyped i
	@:extern private static inline function ofNative(i:NativeInt64):haxe.Int64 return untyped i
	@:extern private static inline function mkNative(i:Dynamic):NativeInt64 return i
	
	#if haxe3
	
	public static inline function make( high : Int, low : Int ) : haxe.Int64
	{
		return ((cast(high, NativeInt64) << 32 ) | (cast(low, NativeInt64))).ofNative();
	}

	public static inline function getLow( x : haxe.Int64 ) : Int
	{
		return cast (x.asNative() & 0xFFFFFFFF.mkNative());
	}

	public static inline function getHigh( x : haxe.Int64 ) : Int
	{
		return cast(x,NativeInt64) >>> 32;
	}
	
	#else
	
	public static inline function make( high : Int32, low : Int32 ) : haxe.Int64
	{
		return ((cast(high, NativeInt64) << 32 ) | (cast(low, NativeInt64))).ofNative();
	}

	public static inline function ofInt32( x : Int32 ) : haxe.Int64 {
		return cast x;
	}
	
	public static inline function getLow( x : haxe.Int64 ) : Int32
	{
		return cast (x.asNative() & 0xFFFFFFFF.mkNative());
	}

	public static inline function getHigh( x : haxe.Int64 ) : Int32
	{
		return cast(cast(x,NativeInt64) >>> 32, Int32);
	}

	#end

	public static inline function ofInt( x : Int ) : haxe.Int64 {
		return cast x;
	}

	public static inline function toInt( x : haxe.Int64 ) : Int
	{
		return cast x;
	}

	public static inline function add( a : haxe.Int64, b : haxe.Int64 ) : haxe.Int64
	{
		return (a.asNative() + b.asNative()).ofNative();
	}

	public static inline function sub( a : haxe.Int64, b : haxe.Int64 ) : haxe.Int64
	{
		return (a.asNative() - b.asNative()).ofNative();
	}

	public static inline function mul( a : haxe.Int64, b : haxe.Int64 ) : haxe.Int64 {
		return (a.asNative() * b.asNative()).ofNative();
	}

	static function divMod( modulus : haxe.Int64, divisor : haxe.Int64 )
	{
		var q:Int64 = (modulus.asNative() / divisor.asNative()).mkNative().ofNative();
		var m:Int64 = (modulus.asNative() % divisor.asNative()).mkNative().ofNative();
		return { quotient : q, modulus : m };
	}

	public static inline function div( a : haxe.Int64, b : haxe.Int64 ) : haxe.Int64 {
		return (a.asNative() / b.asNative()).mkNative().ofNative();
	}

	public static inline function mod( a : haxe.Int64, b : haxe.Int64 ) : haxe.Int64 {
		return (a.asNative() % b.asNative()).mkNative().ofNative();
	}

	public static inline function shl( a : haxe.Int64, b : Int ) : haxe.Int64 {
		return (a.asNative() << b).ofNative();
	}

	public static inline function shr( a : haxe.Int64, b : Int ) : haxe.Int64 {
		return (a.asNative() >> b).ofNative();
	}

	public static inline function ushr( a : haxe.Int64, b : Int ) : haxe.Int64 {
		return (a.asNative() >>> b).ofNative();
	}

	public static inline function and( a : haxe.Int64, b : haxe.Int64 ) : haxe.Int64
	{
		return (a.asNative() & b.asNative()).ofNative();
	}

	public static inline function or( a : haxe.Int64, b : haxe.Int64 ) : haxe.Int64
	{
		return (a.asNative() | b.asNative()).ofNative();
	}

	public static inline function xor( a : haxe.Int64, b : haxe.Int64 ) : haxe.Int64
	{
		return (a.asNative() ^ b.asNative()).ofNative();
	}

	public static inline function neg( a : haxe.Int64 ) : haxe.Int64
	{
		return (~a.asNative()).ofNative();
	}

	public static inline function isNeg( a : haxe.Int64 ) : Bool
	{
		return (a.asNative() < 0.mkNative());
	}

	public static inline function isZero( a : haxe.Int64 ) : Bool
	{
		return (a.asNative() == 0.mkNative());
	}

	public static inline function compare( a : haxe.Int64, b : haxe.Int64 ) : Int
	{
		return cast (a.asNative() - b.asNative());
	}

	/**
		Compare two Int64 in unsigned mode.
	**/
	public static function ucompare( a : haxe.Int64, b : haxe.Int64 ) : Int
	{
		if (a.asNative() < 0.mkNative())
			return (b.asNative() < 0.mkNative()) ? compare( (~a.asNative()).ofNative(), (~b.asNative()).ofNative()) : 1;
		return (b.asNative() < 0.mkNative()) ? -1 : compare(a, b);
	}

	public static inline function toStr( a : haxe.Int64 ) : String {
		return a + "";
	}
}


