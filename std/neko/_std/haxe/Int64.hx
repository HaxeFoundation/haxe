/*
 * Copyright (C)2005-2013 Haxe Foundation
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

import neko.Lib;
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
		return Int64Defs.make( high, low );

	/**
		Construct an Int64 from two 32-bit words `high` and `low`.
	**/
	@:extern inline public static function make( high:Int32, low:Int32 ) : Int64
		return Int64Defs.make(high,low);

	/**
		Returns an Int64 with the value of the Int `x`.
		`x` is sign-extended to fill 64 bits.
	**/
	@:from @:extern inline public static function ofInt( x : Int ) : Int64
		return Int64Defs.ofInt(x);

	/**
		Returns an Int with the value of the Int64 `x`.
		Throws an exception  if `x` cannot be represented in 32 bits.
	**/
	@:extern inline public static function toInt( x : Int64 ) : Int
		return Int64Defs.toInt(x);

	/**
		Returns the high 32-bit word of `x`.
	**/
	@:extern inline public static function getHigh( x : Int64 ) : Int32
		return Int64Defs.getHigh(x);

	/**
		Returns the low 32-bit word of `x`.
	**/
	@:extern inline public static function getLow( x : Int64 ) : Int32
		return Int64Defs.getLow(x);

	/**
		Returns `true` if `x` is less than zero.
	**/
	@:extern inline public static function isNeg( x : Int64) : Bool
		return Int64Defs.isNeg(x);

	/**
		Returns `true` if `x` is exactly zero.
	**/
	@:extern inline public static function isZero( x : Int64 ) : Bool
		return Int64Defs.isZero(x);

	/**
		Compares `a` and `b` in signed mode.
		Returns a negative value if `a < b`, positive if `a > b`,
		or 0 if `a == b`.
	**/
	@:extern inline public static function compare( a : Int64, b : Int64 ) : Int {
		return Int64Defs.compare(a,b);
	}

	/**
		Compares `a` and `b` in unsigned mode.
		Returns a negative value if `a < b`, positive if `a > b`,
		or 0 if `a == b`.
	**/
	@:extern inline public static function ucompare( a : Int64, b : Int64 ) : Int {
		return Int64Defs.ucompare(a,b);
	}

	/**
		Returns a signed decimal `String` representation of `x`.
	**/
	@:extern inline public static function toStr(x:Int64) : String
		return Int64Defs.toStr(x);

	private function toString() : String
	{
		return Int64Defs.toStr(this);
	}

	/**
		Performs signed integer divison of `dividend` by `divisor`.
		Returns `{ quotient : Int64, modulus : Int64 }`.
	**/
	@:extern inline public static function divMod( dividend : Int64, divisor : Int64 ) : { quotient : Int64, modulus : Int64 }
		return Int64Defs.divMod(dividend,divisor);

	/**
		Returns the negative of `x`.
	**/
	@:op(-A) @:extern inline public static function neg( x : Int64 ) : Int64 {
		return Int64Defs.neg(x);
	}

	@:op(++A) private inline function preIncrement() : Int64 {
		return this = Int64Defs.incr(this);
	}

	@:op(A++) private inline function postIncrement() : Int64 {
		var ret = copy();
		preIncrement();
		return ret;
	}

	@:op(--A) private inline function preDecrement() : Int64 {
		return this = Int64Defs.decr(this);
	}

	@:op(A--) private inline function postDecrement() : Int64 {
		var ret = copy();
		preDecrement();
		return ret;
	}

	/**
		Returns the sum of `a` and `b`.
	**/
	@:op(A+B) @:extern inline public static function add( a : Int64, b : Int64 ) : Int64 {
		return Int64Defs.add(a,b);
	}

	@:op(A + B) @:commutative private static inline function addInt( a : Int64, b : Int ) : Int64
		return add( a, b );

	/**
		Returns `a` minus `b`.
	**/
	@:op(A-B) @:extern inline public static function sub( a : Int64, b : Int64 ) : Int64 {
		return Int64Defs.sub(a,b);
	}

	@:op(A - B) private static inline function subInt( a : Int64, b : Int ) : Int64
		return sub( a, b );

	@:op(A - B) private static inline function intSub( a : Int, b : Int64 ) : Int64
		return sub( a, b );

	/**
		Returns the product of `a` and `b`.
	**/
	@:op(A*B) @:extern inline public static function mul( a : Int64, b : Int64 ) : Int64 {
		return Int64Defs.mul(a,b);
	}

	@:op(A * B) @:commutative private static inline function mulInt( a : Int64, b : Int ) : Int64
		return mul( a, b );

	/**
		Returns the quotient of `a` divided by `b`.
	**/
	@:op(A/B) @:extern inline public static function div( a : Int64, b : Int64 ) : Int64
		return Int64Defs.div(a,b);

	@:op(A / B) private static inline function divInt( a : Int64, b : Int ) : Int64
		return div( a, b );

	@:op(A / B) private static inline function intDiv( a : Int, b : Int64 ) : Int64
		return div( a, b ).toInt();

	/**
		Returns the modulus of `a` divided by `b`.
	**/
	@:op(A%B) @:extern inline public static function mod( a : Int64, b : Int64 ) : Int64
		return Int64Defs.mod(a,b);

	@:op(A % B) private static inline function modInt( a : Int64, b : Int ) : Int64
		return mod( a, b ).toInt();

	@:op(A % B) private static inline function intMod( a : Int, b : Int64 ) : Int64
		return mod( a, b ).toInt();

	/**
		Returns `true` if `a` is equal to `b`.
	**/
	@:op(A == B) @:extern inline public static function eq( a : Int64, b : Int64 ) : Bool
		return Int64Defs.eq(a,b);

	@:op(A == B) @:commutative private static inline function eqInt( a : Int64, b : Int ) : Bool
		return eq( a, b );

	/**
		Returns `true` if `a` is not equal to `b`.
	**/
	@:op(A != B) public static inline function neq( a : Int64, b : Int64 ) : Bool
		return !eq(a,b);

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
		return Int64Defs.complement(a);

	/**
		Returns the bitwise AND of `a` and `b`.
	**/
	@:op(A & B) public static inline function and( a : Int64, b : Int64 ) : Int64
		return Int64Defs.and(a,b);

	/**
		Returns the bitwise OR of `a` and `b`.
	**/
	@:op(A | B) public static inline function or( a : Int64, b : Int64 ) : Int64
		return Int64Defs.or(a,b);

	/**
		Returns the bitwise XOR of `a` and `b`.
	**/
	@:op(A ^ B) public static inline function xor( a : Int64, b : Int64 ) : Int64
		return Int64Defs.xor(a,b);

	/**
		Returns `a` left-shifted by `b` bits.
	**/
	@:op(A << B) public static inline function shl( a : Int64, b : Int ) : Int64 {
		return Int64Defs.shl(a,b);
	}

	/**
		Returns `a` right-shifted by `b` bits in signed mode.
		`a` is sign-extended.
	**/
	@:op(A >> B) public static inline function shr( a : Int64, b : Int) : Int64 {
		return Int64Defs.shr(a,b);
	}

	/**
		Returns `a` right-shifted by `b` bits in unsigned mode.
		`a` is padded with zeroes.
	**/
	@:op(A >>> B) public static inline function ushr( a : Int64, b : Int ) : Int64 {
		return Int64Defs.ushr(a,b);
	}

	private var high(get,set) : Int32;
	private inline function get_high() return Int64Defs.getHigh(this);
	private inline function set_high(x) return Int64Defs.setHigh(this,x);

	private var low(get,set) : Int32;
	private inline function get_low() return Int64Defs.getLow(this);
	private inline function set_low(x) return Int64Defs.setLow(this,x);

	@:extern inline private function t()
		return this;
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

@:access(haxe.Int64) private class Int64Defs
{
	public static dynamic function make( high:Int32, low:Int32 ) : Int64
		return new Int64( new __Int64(high, low) );

	public static dynamic function ofInt( x : Int ) : Int64
		return make( x >> 31, x );

	public static dynamic function toInt( x : Int64 ) : Int {
		if( x.t().high != x.t().low >> 31 )
			throw "Overflow";

		return x.t().low;
	}

	public static dynamic function getHigh( x : Int64 ) : Int32
		return x.t().high;

	public static dynamic function getLow( x : Int64 ) : Int32
		return x.t().low;

	public static dynamic function isNeg( x : Int64) : Bool
		return x.t().high < 0;

	public static dynamic function isZero( x : Int64 ) : Bool
		return x == 0;

	public static dynamic function compare( a : Int64, b : Int64 ) : Int {
		var v = a.t().high - b.t().high;
		v = if( v != 0 ) v else Int32.ucompare(a.t().low, b.t().low);
		return a.t().high < 0 ? (b.t().high < 0 ? v : -1) : (b.t().high >= 0 ? v : 1);
	}

	public static dynamic function ucompare( a : Int64, b : Int64 ) : Int {
		var v = Int32.ucompare(a.t().high, b.t().high);
		return if( v != 0 ) v else Int32.ucompare(a.t().low, b.t().low);
	}

	public static dynamic function toStr(i:Int64) : String {
		if ( i == 0 )
			return "0";
		var str = "";
		var neg = false;
		if( i.isNeg() ) {
			neg = true;
			i = -i;
		}
		var ten : Int64 = 10;
		while( i != 0 ) {
			var r = i.divMod( ten );
			str = r.modulus.t().low + str;
			i = r.quotient;
		}
		if( neg ) str = "-" + str;
		return str;
	}

	public static dynamic function divMod( dividend : Int64, divisor : Int64 ) : { quotient : Int64, modulus : Int64 }
	{
		// Handle special cases of 0 and 1
		if( divisor.t().high == 0 )
		{
			switch( divisor.t().low ) {
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

	public static dynamic function neg( x : Int64 ) : Int64 {
		var high = ~x.t().high;
		var low = -x.t().low;
		if( low == 0 )
			high++;
		return make( high, low );
	}

	public static dynamic function decr( x : Int64 ) : Int64 {
		if (x.t().low == 0) x.t().high--;
		x.t().low--;
		return x;
	}

	public static dynamic function incr( x : Int64 ) : Int64 {
		x.t().low++;
		if (x.t().low == 0) x.t().high++;
		return x;
	}

	public static dynamic function add( a : Int64, b : Int64 ) : Int64 {
		var high = a.t().high + b.t().high;
		var low = a.t().low + b.t().low;
		if( Int32.ucompare( low, a.t().low ) < 0 ) high++;
		return make( high, low );
	}

	public static dynamic function sub( a : Int64, b : Int64 ) : Int64 {
		var high = a.t().high - b.t().high;
		var low = a.t().low - b.t().low;
		if( Int32.ucompare( a.t().low, b.t().low ) < 0 ) high--;
        return make( high, low );
	}

	public static dynamic function mul( a : Int64, b : Int64 ) : Int64 {
		var mask = 0xFFFF;
		var al = a.t().low & mask, ah = a.t().low >>> 16;
		var bl = b.t().low & mask, bh = b.t().low >>> 16;
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
		high += a.t().low * b.t().high + a.t().high * b.t().low;
		return make( high, low );
	}

	public static dynamic function div( a : Int64, b : Int64 ) : Int64
		return divMod(a, b).quotient;

	public static dynamic function mod( a : Int64, b : Int64 ) : Int64
		return divMod(a, b).modulus;

	public static dynamic function eq( a : Int64, b : Int64 ) : Bool
		return a.t().high == b.t().high && a.t().low == b.t().low;

	public static dynamic function complement( a : Int64 ) : Int64
		return make( ~a.t().high, ~a.t().low );

	public static dynamic function and( a : Int64, b : Int64 ) : Int64
		return make( a.t().high & b.t().high, a.t().low & b.t().low );

	public static dynamic function or( a : Int64, b : Int64 ) : Int64
		return make( a.t().high | b.t().high, a.t().low | b.t().low );

	public static dynamic function xor( a : Int64, b : Int64 ) : Int64
		return make( a.t().high ^ b.t().high, a.t().low ^ b.t().low );

	public static dynamic function shl( a : Int64, b : Int ) : Int64 {
		b &= 63;
		return if( b == 0 ) a.copy()
			else if( b < 32 ) make( (a.t().high << b) | (a.t().low >>> (32-b)), a.t().low << b)
			else make( a.t().low << (b-32), 0 );
	}

	public static dynamic function shr( a : Int64, b : Int) : Int64 {
		b &= 63;
		return if( b == 0 ) a.copy()
			else if( b < 32 ) make( a.t().high >> b, (a.t().high << (32-b)) | (a.t().low >>> b) )
			else make( a.t().high >> 31, a.t().high >> (b - 32) );
	}

	public static dynamic function ushr( a : Int64, b : Int ) : Int64 {
		b &= 63;
		return if( b == 0 ) a.copy()
			else if( b < 32 ) make( a.t().high >>> b, (a.t().high << (32-b)) | (a.t().low >>> b) )
			else make( 0, a.t().high >>> (b - 32) );
	}

	public static dynamic function setHigh( a : Int64, v : Int32 ) : Int32
		return a.t().high = v;

	public static dynamic function setLow( a : Int64, v : Int32 ) : Int32
		return a.t().low = v;

	static function __init__()
	{
		var mk = try Lib.load('std','int64_make',2) catch(e:Dynamic) null;
		if (mk != null)
		{
			make = mk;
			ofInt = Lib.load('std','int64_new',1); //( x : Int ) : Int64
			toInt = Lib.load('std','int64_to_int',1); //( x : Int64 ) : Int
			getHigh = Lib.load('std','int64_get_high',1); //( x : Int64 ) : Int32
			getLow = Lib.load('std','int64_get_low',1); //( x : Int64 ) : Int32
			isNeg = function(x:Int64) return compare(x,untyped 0) < 0;
			isZero = function(x:Int64) return compare(x,untyped 0) == 0;
			compare = Lib.load('std','int64_compare',2); //( a : Int64, b : Int64 ) : Int {
			ucompare = Lib.load('std','int64_ucompare',2); //( a : Int64, b : Int64 ) : Int {
			nekoToStr = Lib.load('std','int64_to_string',1); //(i:Int64) : String {
			toStr = function(v) return neko.NativeString.toString( nekoToStr(v) );
			divMod = function(a:Int64, b:Int64) return { quotient: div(a,b), modulus: mod(a,b) };
			neg = Lib.load('std','int64_neg',1); //( x : Int64 ) : Int64 {
			decr = function(x) return sub(x,untyped 1);
			incr = function(x) return add(x,untyped 1);
			add = Lib.load('std','int64_add',2); //( a : Int64, b : Int64 ) : Int64 {
			sub = Lib.load('std','int64_sub',2); //( a : Int64, b : Int64 ) : Int64 {
			mul = Lib.load('std','int64_mul',2); //( a : Int64, b : Int64 ) : Int64 {
			div = Lib.load('std','int64_div',2); //( a : Int64, b : Int64 ) : Int64
			mod = Lib.load('std','int64_mod',2); //( a : Int64, b : Int64 ) : Int64
			eq = function(a,b) return compare(a,b) == 0;
			complement = Lib.load('std','int64_complement',1); //( a : Int64 ) : Int64
			and = Lib.load('std','int64_and',2); //( a : Int64, b : Int64 ) : Int64
			or = Lib.load('std','int64_or',2); //( a : Int64, b : Int64 ) : Int64
			xor = Lib.load('std','int64_xor',2); //( a : Int64, b : Int64 ) : Int64
			shl = Lib.load('std','int64_shl',2); //( a : Int64, b : Int ) : Int64 {
			shr = Lib.load('std','int64_shr',2); //( a : Int64, b : Int) : Int64 {
			ushr = Lib.load('std','int64_ushr',2); //( a : Int64, b : Int ) : Int64 {

			replace = Lib.load('std','int64_replace',3);
			setHigh = function(i64,val) {replace(i64,val,getLow(i64)); return val;}
			setLow = function(i64,val) {replace(i64,getHigh(i64),val); return val;}
		}
	}

	private static var replace:Int64->Int32->Int32->Int64;
	private static var nekoToStr:Int64->neko.NativeString;
}
