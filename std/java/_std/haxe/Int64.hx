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
using haxe.Int64;

@:coreType @:notNull @:runtimeValue private abstract NativeInt64 from Int to Int {}

private typedef __Int64 = NativeInt64;

@:coreApi
@:nativeGen
abstract Int64(__Int64) from __Int64 to __Int64
{
	private inline function new(x : __Int64)
		this = x;

	public inline function copy():Int64
		return new Int64( this );

	public static inline function make( high : Int32, low : Int32 ) : Int64
		return new Int64( (cast(high, NativeInt64) << 32) | (low & untyped __java__('0xffffffffL')) );

	@:from public static inline function ofInt( x : Int ) : Int64
		return cast x;

	public static inline function toInt( x : Int64 ) : Int {
		if( x < 0x80000000 || x > 0x7FFFFFFF )
			throw "Overflow";
		return cast x;
	}

	public static inline function getHigh( x : Int64 ) : Int32
		return cast( x >> 32, Int );

	public static inline function getLow( x : Int64 ) : Int32
		return cast( x, Int );

	public static inline function isNeg( x : Int64 ) : Bool
		return x < 0;

	public static inline function isZero( x : Int64 ) : Bool
		return x == 0;

	public static inline function compare( a : Int64, b : Int64 ) : Int
	{
		if( a < b ) return -1;
		if( a > b ) return 1;
		return 0;
	}

	public static inline function ucompare( a : Int64, b : Int64 ) : Int {
		if (a < 0)
			return (b < 0) ? compare(a, b) : 1;
		return (b < 0) ? -1 : compare(a, b);
	}

	public static inline function toStr( x : Int64 ) : String
		return x + "";

	public static inline function divMod(dividend:Int64, divisor:Int64) : { quotient : Int64, modulus : Int64 }
		return { quotient: dividend / divisor, modulus: dividend % divisor };

	private function toString() : String
		return this + "";

	@:op(-A) private static function _neg( x : Int64 ) : Int64;

	public static function neg( x : Int64 ) : Int64
		return -x;

	@:op(++A) private function preIncrement() : Int64;
	@:op(A++) private function postIncrement() : Int64;
	@:op(--A) private function preDecrement() : Int64;
	@:op(A--) private function postDecrement() : Int64;
	
	@:op(A + B) private static function _add( a : Int64, b : Int64 ) : Int64;
	@:op(A + B) @:commutative private static function _addInt( a : Int64, b : Int ) : Int64;

	public static inline function add( a : Int64, b : Int64 ) : Int64
		return a + b;

	@:op(A - B) private static function _sub( a : Int64, b : Int64 ) : Int64;
	@:op(A - B) private static function _subInt( a : Int64, b : Int ) : Int64;
	@:op(A - B) private static function _intSub( a : Int, b : Int64 ) : Int64;

	public static inline function sub( a : Int64, b : Int64 ) : Int64
		return a - b;

	@:op(A * B) private static function _mul( a : Int64, b : Int64 ) : Int64;
	@:op(A * B) @:commutative private static function _mulInt( a : Int64, b : Int ) : Int64;

	public static inline function mul( a : Int64, b : Int64 ) : Int64
		return a * b;

	@:op(A / B) public static inline function div( a : Int64, b : Int64 ) : Int64
		return cast( (a : NativeInt64) / (b : NativeInt64) );

	@:op(A / B) private static inline function divInt( a : Int64, b : Int ) : Int64
		return cast( (a : NativeInt64) / (b : NativeInt64) );

	@:op(A / B) private static inline function intDiv( a : Int, b : Int64 ) : Int64
		return cast( (a : NativeInt64) / (b : NativeInt64) );

	@:op(A % B) private static function _mod( a : Int64, b : Int64 ) : Int64;
	@:op(A % B) private static function _modInt( a : Int64, b : Int ) : Int64;
	@:op(A % B) private static function _intMod( a : Int, b : Int64 ) : Int64;

	public static inline function mod( a : Int64, b : Int64 ) : Int64
		return a % b;

	@:op(A == B) private static function _eq( a : Int64, b : Int64 ) : Bool;
	@:op(A == B) @:commutative private static function _eqInt( a : Int64, b : Int ) : Bool;

	public static inline function eq( a : Int64, b : Int64 ) : Bool
		return a == b;

	@:op(A != B) private static function _neq( a : Int64, b : Int64 ) : Bool;
	@:op(A != B) @:commutative private static function _neqInt( a : Int64, b : Int ) : Bool;

	public static inline function neq( a : Int64, b : Int64 ) : Bool
		return a != b;

	@:op(A < B) private static function lt( a : Int64, b : Int64 ) : Bool;
	@:op(A < B) private static function ltInt( a : Int64, b : Int ) : Bool;
	@:op(A < B) private static function intLt( a : Int, b : Int64 ) : Bool;

	@:op(A <= B) private static function lte( a : Int64, b : Int64 ) : Bool;
	@:op(A <= B) private static function lteInt( a : Int64, b : Int ) : Bool;
	@:op(A <= B) private static function intLte( a : Int, b : Int64 ) : Bool;

	@:op(A > B) private static function gt( a : Int64, b : Int64 ) : Bool;
	@:op(A > B) private static function gtInt( a : Int64, b : Int ) : Bool;
	@:op(A > B) private static function intGt( a : Int, b : Int64 ) : Bool;

	@:op(A >= B) private static function gte( a : Int64, b : Int64 ) : Bool;
	@:op(A >= B) private static function gteInt( a : Int64, b : Int ) : Bool;
	@:op(A >= B) private static function intGte( a : Int, b : Int64 ) : Bool;

	@:op(~A) private static function complement( x : Int64 ) : Int64;

	@:op(A & B) private static function _and( a : Int64, b : Int64 ) : Int64;

	public static inline function and( a : Int64, b : Int64 ) : Int64
		return a & b;

	@:op(A | B) private static function _or( a : Int64, b : Int64 ) : Int64;

	public static inline function or( a : Int64, b : Int64 ) : Int64
		return a | b;

	@:op(A ^ B) private static function _xor( a : Int64, b : Int64 ) : Int64;

	public static inline function xor( a : Int64, b : Int64 ) : Int64
		return a ^ b;

	@:op(A << B) public static inline function shl( a : Int64, b : Int ) : Int64
		return a << b;

	@:op(A >> B) public static inline function shr( a : Int64, b : Int ) : Int64
		return a >> b;

	@:op(A >>> B) public static inline function ushr( a : Int64, b : Int ) : Int64
		return a >>> b;
}