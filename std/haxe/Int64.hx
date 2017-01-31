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
package haxe;

using haxe.Int64;

 /**
	A cross-platform signed 64-bit integer.
	Int64 instances can be created from two 32-bit words using `Int64.make()`.
 **/
#if flash
@:notNull
#end
abstract Int64(__Int64) from __Int64 to __Int64
{
	private inline function new( x : __Int64 )
		this = x;

	/**
		Makes a copy of `this` Int64.
	**/
	public inline function copy():Int64
		return make( high, low );

	/**
		Construct an Int64 from two 32-bit words `high` and `low`.
	**/
	public static inline function make( high:Int32, low:Int32 ) : Int64
		return new Int64( new __Int64(high, low) );

	/**
		Returns an Int64 with the value of the Int `x`.
		`x` is sign-extended to fill 64 bits.
	**/
	@:from public static inline function ofInt( x : Int ) : Int64
#if lua
		return make( (x:Int32) >> 31, (x:Int32));
#else
		return make( x >> 31, x );
#end

	/**
		Returns an Int with the value of the Int64 `x`.
		Throws an exception  if `x` cannot be represented in 32 bits.
	**/
	public static inline function toInt( x : Int64 ) : Int {
		if( x.high != x.low >> 31 )
			throw "Overflow";

		return x.low;
	}

	/**
		Returns whether the value `val` is of type `haxe.Int64`
	**/
	inline public static function is( val : Dynamic ) : Bool
		return Std.is(val,__Int64);

	/**
		Returns the high 32-bit word of `x`.
	**/
	@:deprecated("Use high instead")
	public static inline function getHigh( x : Int64 ) : Int32
		return x.high;

	/**
		Returns the low 32-bit word of `x`.
	**/
	@:deprecated("Use low instead")
	public static inline function getLow( x : Int64 ) : Int32
		return x.low;

	/**
		Returns `true` if `x` is less than zero.
	**/
	public static inline function isNeg( x : Int64) : Bool
		return x.high < 0;

	/**
		Returns `true` if `x` is exactly zero.
	**/
	public static inline function isZero( x : Int64 ) : Bool
		return x == 0;

	/**
		Compares `a` and `b` in signed mode.
		Returns a negative value if `a < b`, positive if `a > b`,
		or 0 if `a == b`.
	**/
	public static inline function compare( a : Int64, b : Int64 ) : Int {
		var v = a.high - b.high;
		v = if( v != 0 ) v else Int32.ucompare(a.low, b.low);
		return a.high < 0 ? (b.high < 0 ? v : -1) : (b.high >= 0 ? v : 1);
	}

	/**
		Compares `a` and `b` in unsigned mode.
		Returns a negative value if `a < b`, positive if `a > b`,
		or 0 if `a == b`.
	**/
	public static inline function ucompare( a : Int64, b : Int64 ) : Int {
		var v = Int32.ucompare(a.high, b.high);
		return if( v != 0 ) v else Int32.ucompare(a.low, b.low);
	}

	/**
		Returns a signed decimal `String` representation of `x`.
	**/
	public static inline function toStr(x:Int64) : String
		return x.toString();

	#if as3 public #else private #end function toString() : String
	{
		var i : Int64 = cast this;
		if ( i == 0 )
			return "0";
		var str = "";
		var neg = false;
		if( i.isNeg() ) {
			neg = true;
			// i = -i; cannot negate here as --9223372036854775808 = -9223372036854775808
		}
		var ten : Int64 = 10;
		while( i != 0 ) {
			var r = i.divMod( ten );
			if (r.modulus.isNeg()) {
				str = Int64.neg(r.modulus).low + str;
				i = Int64.neg(r.quotient);
			} else {
				str = r.modulus.low + str;
				i = r.quotient;
			}
		}
		if( neg ) str = "-" + str;
		return str;
	}

	public static inline function parseString( sParam : String ) : Int64 {
		return Int64Helper.parseString( sParam );
	}

	public static inline function fromFloat( f : Float ) : Int64 {
		return Int64Helper.fromFloat( f );
	}

	/**
		Performs signed integer divison of `dividend` by `divisor`.
		Returns `{ quotient : Int64, modulus : Int64 }`.
	**/
	public static function divMod( dividend : Int64, divisor : Int64 ) : { quotient : Int64, modulus : Int64 }
	{
		// Handle special cases of 0 and 1
		if( divisor.high == 0 )
		{
			switch( divisor.low ) {
				case 0: throw "divide by zero";
				case 1: return { quotient : dividend.copy(), modulus : 0 };
			}
		}

		var divSign = dividend.isNeg() != divisor.isNeg();

		var modulus = dividend.isNeg() ? -dividend : dividend.copy();
		divisor = divisor.isNeg() ? -divisor : divisor;

		var quotient : Int64 = 0;
		var mask : Int64 = 1;

		while( !divisor.isNeg() ) {
			var cmp = ucompare( divisor, modulus );
			divisor <<= 1;
			mask <<= 1;
			if( cmp >= 0 ) break;
		}

		while( mask != 0 ) {
			if( ucompare(modulus, divisor) >= 0 ) {
				quotient |= mask;
				modulus -= divisor;
			}
			mask >>>= 1;
			divisor >>>= 1;
		}

		if( divSign ) quotient = -quotient;
		if( dividend.isNeg() ) modulus = -modulus;

		return {
			quotient : quotient,
			modulus  : modulus
		};
	}

	/**
		Returns the negative of `x`.
	**/
	@:op(-A) public static inline function neg( x : Int64 ) : Int64 {
		var high = ~x.high;
		var low = -x.low;
		if( low == 0 )
			high++;
		return make( high, low );
	}

	@:op(++A) private inline function preIncrement() : Int64 {
		this = copy();
		this.low++;
		if( this.low == 0 ) this.high++;
		return cast this;
	}

	@:op(A++) private inline function postIncrement() : Int64 {
		var ret = this;
		preIncrement();
		return ret;
	}

	@:op(--A) private inline function preDecrement() : Int64 {
		this = copy();
		if( this.low == 0 ) this.high--;
		this.low--;
		return cast this;
	}

	@:op(A--) private inline function postDecrement() : Int64 {
		var ret = this;
		preDecrement();
		return ret;
	}

	/**
		Returns the sum of `a` and `b`.
	**/
	@:op(A + B) public static inline function add( a : Int64, b : Int64 ) : Int64 {
		var high = a.high + b.high;
		var low = a.low + b.low;
		if( Int32.ucompare( low, a.low ) < 0 ) high++;
		return make( high, low );
	}

	@:op(A + B) @:commutative private static inline function addInt( a : Int64, b : Int ) : Int64
		return add( a, b );

	/**
		Returns `a` minus `b`.
	**/
	@:op(A - B) public static inline function sub( a : Int64, b : Int64 ) : Int64 {
		var high = a.high - b.high;
		var low = a.low - b.low;
		if( Int32.ucompare( a.low, b.low ) < 0 ) high--;
        return make( high, low );
	}

	@:op(A - B) private static inline function subInt( a : Int64, b : Int ) : Int64
		return sub( a, b );

	@:op(A - B) private static inline function intSub( a : Int, b : Int64 ) : Int64
		return sub( a, b );

	/**
		Returns the product of `a` and `b`.
	**/
	@:op(A * B) public static #if !lua inline #end function mul( a : Int64, b : Int64 ) : Int64 {
		var mask = 0xFFFF;
		var al = a.low & mask, ah = a.low >>> 16;
		var bl = b.low & mask, bh = b.low >>> 16;
		var p00 = al * bl;
		var p10 = ah * bl;
		var p01 = al * bh;
		var p11 = ah * bh;
		var low = p00;
		var high = p11 + (p01 >>> 16) + (p10 >>> 16);
		p01 <<= 16;
		low += p01;
		if( Int32.ucompare(low, p01) < 0 ) high++;
		p10 <<= 16;
		low += p10;
		if( Int32.ucompare(low, p10) < 0 ) high++;
		high += a.low * b.high + a.high * b.low;
		return make( high, low );
	}

	@:op(A * B) @:commutative private static inline function mulInt( a : Int64, b : Int ) : Int64
		return mul( a, b );

	/**
		Returns the quotient of `a` divided by `b`.
	**/
	@:op(A / B) public static inline function div( a : Int64, b : Int64 ) : Int64
		return divMod(a, b).quotient;

	@:op(A / B) private static inline function divInt( a : Int64, b : Int ) : Int64
		return div( a, b );

	@:op(A / B) private static inline function intDiv( a : Int, b : Int64 ) : Int64
		return div( a, b ).toInt();

	/**
		Returns the modulus of `a` divided by `b`.
	**/
	@:op(A % B) public static inline function mod( a : Int64, b : Int64 ) : Int64
		return divMod(a, b).modulus;

	@:op(A % B) private static inline function modInt( a : Int64, b : Int ) : Int64
		return mod( a, b ).toInt();

	@:op(A % B) private static inline function intMod( a : Int, b : Int64 ) : Int64
		return mod( a, b ).toInt();

	/**
		Returns `true` if `a` is equal to `b`.
	**/
	@:op(A == B) public static inline function eq( a : Int64, b : Int64 ) : Bool
		return a.high == b.high && a.low == b.low;

	@:op(A == B) @:commutative private static inline function eqInt( a : Int64, b : Int ) : Bool
		return eq( a, b );

	/**
		Returns `true` if `a` is not equal to `b`.
	**/
	@:op(A != B) public static inline function neq( a : Int64, b : Int64 ) : Bool
		return a.high != b.high || a.low != b.low;

	@:op(A != B) @:commutative private static inline function neqInt( a : Int64, b : Int ) : Bool
		return neq(a, b);

	@:op(A < B) private static inline function lt( a : Int64, b : Int64 ) : Bool
		return compare(a, b) < 0;

	@:op(A < B) private static inline function ltInt( a : Int64, b : Int ) : Bool
		return lt(a, b);

	@:op(A < B) private static inline function intLt( a : Int, b : Int64 ) : Bool
		return lt(a, b);

	@:op(A <= B) private static inline function lte( a : Int64, b : Int64 ) : Bool
		return compare(a, b) <= 0;

	@:op(A <= B) private static inline function lteInt( a : Int64, b : Int ) : Bool
		return lte(a, b);

	@:op(A <= B) private static inline function intLte( a : Int, b : Int64 ) : Bool
		return lte(a, b);

	@:op(A > B) private static inline function gt( a : Int64, b : Int64 ) : Bool
		return compare(a, b) > 0;

	@:op(A > B) private static inline function gtInt( a : Int64, b : Int ) : Bool
		return gt(a, b);

	@:op(A > B) private static inline function intGt( a : Int, b : Int64 ) : Bool
		return gt( a, b );

	@:op(A >= B) private static inline function gte( a : Int64, b : Int64 ) : Bool
		return compare(a, b) >= 0;

	@:op(A >= B) private static inline function gteInt( a : Int64, b : Int ) : Bool
		return gte(a, b);

	@:op(A >= B) private static inline function intGte( a : Int, b : Int64 ) : Bool
		return gte(a, b);

	/**
		Returns the bitwise NOT of `a`.
	**/
	@:op(~A) private static inline function complement( a : Int64 ) : Int64
		return make( ~a.high, ~a.low );

	/**
		Returns the bitwise AND of `a` and `b`.
	**/
	@:op(A & B) public static inline function and( a : Int64, b : Int64 ) : Int64
		return make( a.high & b.high, a.low & b.low );

	/**
		Returns the bitwise OR of `a` and `b`.
	**/
	@:op(A | B) public static inline function or( a : Int64, b : Int64 ) : Int64
		return make( a.high | b.high, a.low | b.low );

	/**
		Returns the bitwise XOR of `a` and `b`.
	**/
	@:op(A ^ B) public static inline function xor( a : Int64, b : Int64 ) : Int64
		return make( a.high ^ b.high, a.low ^ b.low );

	/**
		Returns `a` left-shifted by `b` bits.
	**/
	@:op(A << B) public static inline function shl( a : Int64, b : Int ) : Int64 {
		b &= 63;
		return if( b == 0 ) a.copy()
			else if( b < 32 ) make( (a.high << b) | (a.low >>> (32-b)), a.low << b)
			else make( a.low << (b-32), 0 );
	}

	/**
		Returns `a` right-shifted by `b` bits in signed mode.
		`a` is sign-extended.
	**/
	@:op(A >> B) public static inline function shr( a : Int64, b : Int) : Int64 {
		b &= 63;
		return if( b == 0 ) a.copy()
			else if( b < 32 ) make( a.high >> b, (a.high << (32-b)) | (a.low >>> b) );
			else make( a.high >> 31, a.high >> (b - 32) );
	}

	/**
		Returns `a` right-shifted by `b` bits in unsigned mode.
		`a` is padded with zeroes.
	**/
	@:op(A >>> B) public static inline function ushr( a : Int64, b : Int ) : Int64 {
		b &= 63;
		return if( b == 0 ) a.copy()
			else if( b < 32 ) make( a.high >>> b, (a.high << (32-b)) | (a.low >>> b) );
			else make( 0, a.high >>> (b - 32) );
	}

	public var high(get, never) : Int32;
	private inline function get_high() return this.high;
	private inline function set_high(x) return this.high = x;

	public var low(get, never) : Int32;
	private inline function get_low() return this.low;
	private inline function set_low(x) return this.low = x;
}

/**
  * This typedef will fool @:coreApi into thinking that we are using
  * the same underlying type, even though it might be different on
  * specific platforms.
  */
private typedef __Int64 = ___Int64;

private class ___Int64 {
	public var high : Int32;
	public var low : Int32;

	public inline function new( high, low ) {
		this.high = high;
		this.low = low;
	}

	/**
		We also define toString here to ensure we always get a pretty string
		when tracing or calling Std.string. This tends not to happen when
		toString is only in the abstract.
	**/
	public function toString() : String
		return Int64.toStr( cast this );
}
