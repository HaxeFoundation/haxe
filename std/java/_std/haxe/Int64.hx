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

private typedef __Int64 = java.StdTypes.Int64;

@:coreApi
abstract Int64(__Int64) from __Int64 to __Int64
{

	@:extern public static inline function make( high : Int32, low : Int32 ) : Int64
		return new Int64( (cast(high, __Int64) << 32) | (cast(low, __Int64)& untyped __java__('0xffffffffL')) );

	@:extern private inline function new(x : __Int64)
		this = x;

	private var val( get, set ) : __Int64;
	@:extern inline function get_val() : __Int64 return this;
	@:extern inline function set_val( x : __Int64 ) : __Int64 return this = x;

	public var high( get, never ):Int32;
	@:extern public inline function get_high():Int32 return cast(this >> 32);

	public var low( get, never ):Int32;
	@:extern public inline function get_low():Int32 return cast this;

	@:extern public inline function copy():Int64
		return new Int64( this );

	@:extern @:from public static inline function ofInt( x : Int ) : Int64
		return cast x;

	@:extern public static inline function toInt( x : Int64 ) : Int {
		if( x.val < 0x80000000 || x.val > 0x7FFFFFFF )
			throw "Overflow";
		return cast x.val;
	}

	@:extern public static inline function getHigh( x : Int64 ) : Int32
		return cast( x.val >> 32 );

	@:extern public static inline function getLow( x : Int64 ) : Int32
		return cast( x.val );

	@:extern public static inline function isNeg( x : Int64 ) : Bool
		return x.val < 0;

	@:extern public static inline function isZero( x : Int64 ) : Bool
		return x.val == 0;

	@:extern public static inline function compare( a : Int64, b : Int64 ) : Int
	{
		if( a.val < b.val ) return -1;
		if( a.val > b.val ) return 1;
		return 0;
	}

	@:extern public static inline function ucompare( a : Int64, b : Int64 ) : Int {
		if( a.val < 0 )
			return ( b.val < 0 ) ? compare( a, b ) : 1;
		return ( b.val < 0 ) ? -1 : compare( a, b );
	}

	@:extern public static inline function toStr( x : Int64 ) : String
		return '${x.val}';

	@:extern public static inline function divMod( dividend : Int64, divisor : Int64 ) : { quotient : Int64, modulus : Int64 }
		return { quotient: dividend / divisor, modulus: dividend % divisor };

	@:extern private inline function toString() : String
		return '$this';

	@:extern @:op(-A) public inline static function neg( x : Int64 ) : Int64
		return -x.val;

	@:extern @:op(++A) private inline function preIncrement() : Int64
		return ++this;

	@:extern @:op(A++) private inline function postIncrement() : Int64
		return this++;

	@:extern @:op(--A) private inline function preDecrement() : Int64
		return --this;

	@:extern @:op(A--) private inline function postDecrement() : Int64
		return this--;

	@:extern @:op(A + B) public static inline function add( a : Int64, b : Int64 ) : Int64
		return a.val + b.val;

	@:extern @:op(A + B) @:commutative private static inline function addInt( a : Int64, b : Int ) : Int64
		return a.val + b;

	@:extern @:op(A - B) public static inline function sub( a : Int64, b : Int64 ) : Int64
		return a.val - b.val;

	@:extern @:op(A - B) private static inline function subInt( a : Int64, b : Int ) : Int64
		return a.val - b;

	@:extern @:op(A - B) private static inline function intSub( a : Int, b : Int64 ) : Int64
		return a - b.val;

	@:extern @:op(A * B) public static inline function mul( a : Int64, b : Int64 ) : Int64
		return a.val * b.val;

	@:extern @:op(A * B) @:commutative private static inline function mulInt( a : Int64, b : Int ) : Int64
		return a.val * b;

	@:extern @:op(A / B) public static inline function div( a : Int64, b : Int64 ) : Int64
		return a.val / b.val;

	@:extern @:op(A / B) private static inline function divInt( a : Int64, b : Int ) : Int64
		return a.val / b;

	@:extern @:op(A / B) private static inline function intDiv( a : Int, b : Int64 ) : Int64
		return a / b.val;

	@:extern @:op(A % B) public static inline function mod( a : Int64, b : Int64 ) : Int64
		return a.val % b.val;

	@:extern @:op(A % B) private static inline function modInt( a : Int64, b : Int ) : Int64
		return a.val % b;

	@:extern @:op(A % B) private static inline function intMod( a : Int, b : Int64 ) : Int64
		return a % b.val;

	@:extern @:op(A == B) public static inline function eq( a : Int64, b : Int64 ) : Bool
		return a.val == b.val;

	@:extern @:op(A == B) @:commutative private static inline function eqInt( a : Int64, b : Int ) : Bool
		return a.val == b;

	@:extern @:op(A != B) public static inline function neq( a : Int64, b : Int64 ) : Bool
		return a.val != b.val;

	@:extern @:op(A != B) @:commutative private static inline function neqInt( a : Int64, b : Int ) : Bool
		return a.val != b;

	@:extern @:op(A < B) private static inline function lt( a : Int64, b : Int64 ) : Bool
		return a.val < b.val;

	@:extern @:op(A < B) private static inline function ltInt( a : Int64, b : Int ) : Bool
		return a.val < b;

	@:extern @:op(A < B) private static inline function intLt( a : Int, b : Int64 ) : Bool
		return a < b.val;

	@:extern @:op(A <= B) private static inline function lte( a : Int64, b : Int64 ) : Bool
		return a.val <= b.val;

	@:extern @:op(A <= B) private static inline function lteInt( a : Int64, b : Int ) : Bool
		return a.val <= b;

	@:extern @:op(A <= B) private static inline function intLte( a : Int, b : Int64 ) : Bool
		return a <= b.val;

	@:extern @:op(A > B) private static inline function gt( a : Int64, b : Int64 ) : Bool
		return a.val > b.val;

	@:extern @:op(A > B) private static inline function gtInt( a : Int64, b : Int ) : Bool
		return a.val > b;

	@:extern @:op(A > B) private static inline function intGt( a : Int, b : Int64 ) : Bool
		return a > b.val;

	@:extern @:op(A >= B) private static inline function gte( a : Int64, b : Int64 ) : Bool
		return a.val >= b.val;

	@:extern @:op(A >= B) private static inline function gteInt( a : Int64, b : Int ) : Bool
		return a.val >= b;

	@:extern @:op(A >= B) private static inline function intGte( a : Int, b : Int64 ) : Bool
		return a >= b.val;

	@:extern @:op(~A) private static inline function complement( x : Int64 ) : Int64
		return ~x.val;

	@:extern @:op(A & B) public static inline function and( a : Int64, b : Int64 ) : Int64
		return a.val & b.val;

	@:extern @:op(A | B) public static inline function or( a : Int64, b : Int64 ) : Int64
		return a.val | b.val;

	@:extern @:op(A ^ B) public static inline function xor( a : Int64, b : Int64 ) : Int64
		return a.val ^ b.val;

	@:extern @:op(A << B) public static inline function shl( a : Int64, b : Int ) : Int64
		return a.val << b;

	@:extern @:op(A >> B) public static inline function shr( a : Int64, b : Int ) : Int64
		return a.val >> b;

	@:extern @:op(A >>> B) public static inline function ushr( a : Int64, b : Int ) : Int64
		return a.val >>> b;
}
