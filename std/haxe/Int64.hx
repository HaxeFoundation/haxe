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

#if haxe3

class Int64 {

	var high : Int;
	var low : Int;

	function new(high, low) {
		this.high = i32(high);
		this.low = i32(low);
	}
	
	@:extern inline function i32(i) {
		#if (php || js || flash8)
		return i | 0;
		#else
		return i;
		#end
	}

	#if as3 public #end function toString() {
		if ((high|low) == 0 )
			return "0";
		var str = "";
		var neg = false;
		var i = this;
		if( isNeg(i) ) {
			neg = true;
			i = Int64.neg(i);
		}
		var ten = ofInt(10);
		while( !isZero(i) ) {
			var r = divMod(i, ten);
			str = r.modulus.low + str;
			i = r.quotient;
		}
		if( neg ) str = "-" + str;
		return str;
	}

	public static inline function make( high : Int, low : Int ) : Int64 {
		return new Int64(high, low);
	}

	public static inline function ofInt( x : Int ) : Int64 {
		return new Int64(x >> 31,x);
	}

	public static function toInt( x : Int64 ) : Int {
		if( x.high != 0 ) {
			if( x.high < 0 )
				return -toInt(neg(x));
			throw "Overflow";
		}
		return x.low;
	}

	public static function getLow( x : Int64 ) : Int {
		return x.low;
	}

	public static function getHigh( x : Int64 ) : Int {
		return x.high;
	}

	public static function add( a : Int64, b : Int64 ) : Int64 {
		var high = a.high + b.high;
		var low = a.low + b.low;
		if( uicompare(low,a.low) < 0 )
			high++;
		return new Int64(high, low);
	}

	public static function sub( a : Int64, b : Int64 ) : Int64 {
		var high = a.high - b.high;
		var low = a.low - b.low;
		if( uicompare(a.low,b.low) < 0 )
			high--;
		return new Int64(high, low);
	}

	public static function mul( a : Int64, b : Int64 ) : Int64 {
		var mask = 0xFFFF;
		var al = a.low & mask, ah = a.low >>> 16;
		var bl = b.low & mask, bh = b.low >>> 16;
		var p00 = al * bl;
		var p10 = ah * bl;
		var p01 = al * bh;
		var p11 = ah * bh;
		var low = p00;
		var high = p11 + (p01 >>> 16) + (p10 >>> 16);
		p01 = (p01 << 16); low += p01; if( uicompare(low,p01) < 0 ) high++;
		p10 = (p10 << 16); low += p10; if( uicompare(low,p10) < 0 ) high++;
		high += a.low * b.high;
		high += a.high * b.low;
		return new Int64(high, low);
	}

	static function divMod( modulus : Int64, divisor : Int64 ) {
		var quotient = new Int64(0, 0);
		var mask = new Int64(0, 1);
		divisor = new Int64(divisor.high, divisor.low);
		while( divisor.high >= 0 ) {
			var cmp = ucompare(divisor, modulus);
			divisor.high = (divisor.high << 1) | (divisor.low >>> 31);
			divisor.low <<= 1;
			mask.high = (mask.high << 1) | (mask.low >>> 31);
			mask.low <<= 1;
			if( cmp >= 0 ) break;
		}
		while( (mask.low | mask.high) != 0 ) {
			if( ucompare(modulus, divisor) >= 0 ) {
				quotient.high |= mask.high;
				quotient.low |= mask.low;
				modulus = sub(modulus,divisor);
			}
			mask.low = (mask.low >>> 1) | (mask.high << 31);
			mask.high >>>= 1;

			divisor.low = (divisor.low >>> 1) | (divisor.high << 31);
			divisor.high >>>= 1;
		}
		return { quotient : quotient, modulus : modulus };
	}

	public static inline function div( a : Int64, b : Int64 ) : Int64 {
		var sign = (a.high | b.high) < 0;
		if( a.high < 0 ) a = neg(a);
		if( b.high < 0 ) b = neg(b);
		var q = divMod(a, b).quotient;
		return sign ? neg(q) : q;
	}

	public static inline function mod( a : Int64, b : Int64 ) : Int64 {
		var sign = (a.high | b.high) < 0;
		if( a.high < 0 ) a = neg(a);
		if( b.high < 0 ) b = neg(b);
		var m = divMod(a, b).modulus;
		return sign ? neg(m) : m;
	}

	public static inline function shl( a : Int64, b : Int ) : Int64 {
		return if( b & 63 == 0 ) a else if( b & 63 < 32 ) new Int64( (a.high << b) | (a.low >>> (32-(b&63))), a.low << b ) else new Int64( a.low << (b - 32), 0 );
	}

	public static inline function shr( a : Int64, b : Int ) : Int64 {
		return if( b & 63 == 0 ) a else if( b & 63 < 32 ) new Int64( a.high >> b, (a.low >>> b) | (a.high << (32 - (b&63))) ) else new Int64( a.high >> 31, a.high >> (b - 32) );
	}

	public static inline function ushr( a : Int64, b : Int ) : Int64 {
		return if( b & 63 == 0 ) a else if( b & 63 < 32 ) new Int64( a.high >>> b, (a.low >>> b) | (a.high << (32 - (b&63))) ) else new Int64( 0, a.high >>> b - 32 );
	}

	public static inline function and( a : Int64, b : Int64 ) : Int64 {
		return new Int64( a.high & b.high, a.low & b.low );
	}

	public static inline function or( a : Int64, b : Int64 ) : Int64 {
		return new Int64( a.high | b.high, a.low | b.low );
	}

	public static inline function xor( a : Int64, b : Int64 ) : Int64 {
		return new Int64( a.high ^ b.high, a.low ^ b.low );
	}

	public static inline function neg( a : Int64 ) : Int64 {
		var high = ~a.high;
		var low = -a.low;
		if( low == 0 )
			high++;
		return new Int64(high,low);
	}

	public static inline function isNeg( a : Int64 ) : Bool {
		return a.high < 0;
	}

	public static inline function isZero( a : Int64 ) : Bool {
		return (a.high | a.low) == 0;
	}

	static function uicompare( a : Int, b : Int ) {
		return a < 0 ? (b < 0 ? ~b - ~a : 1) : (b < 0 ? -1 : a - b);
	}

	public static inline function compare( a : Int64, b : Int64 ) : Int {
		var v = a.high - b.high;
		return if( v != 0 ) v else uicompare(a.low,b.low);
	}

	/**
		Compare two Int64 in unsigned mode.
	**/
	public static inline function ucompare( a : Int64, b : Int64 ) : Int {
		var v = uicompare(a.high,b.high);
		return if( v != 0 ) v else uicompare(a.low, b.low);
	}

	public static inline function toStr( a : Int64 ) : String {
		return a.toString();
	}

}


#else

using haxe.Int32;

class Int64 {

	var high : Int32;
	var low : Int32;

	function new(high, low) {
		this.high = high;
		this.low = low;
	}

	#if as3 public #end function toString() {
		if (high.isZero() && low.isZero())
			return "0";
		var str = "";
		var neg = false;
		var i = this;
		if( isNeg(i) ) {
			neg = true;
			i = Int64.neg(i);
		}
		var ten = ofInt(10);
		while( !isZero(i) ) {
			var r = divMod(i, ten);
			str = r.modulus.low.toInt() + str;
			i = r.quotient;
		}
		if( neg ) str = "-" + str;
		return str;
	}

	public static inline function make( high : Int32, low : Int32 ) : Int64 {
		return new Int64(high, low);
	}

	public static inline function ofInt( x : Int ) : Int64 {
		return new Int64((x >> 31).ofInt(),x.ofInt());
	}

	public static inline function ofInt32( x : Int32 ) : Int64 {
		return new Int64(x.shr(31),x);
	}

	public static function toInt( x : Int64 ) : Int {
		if( x.high.toInt() != 0 ) {
			if( x.high.isNeg() )
				return -toInt(neg(x));
			throw "Overflow";
		}
		return x.low.toInt();
	}

	public static function getLow( x : Int64 ) : Int32 {
		return x.low;
	}

	public static function getHigh( x : Int64 ) : Int32 {
		return x.high;
	}

	public static function add( a : Int64, b : Int64 ) : Int64 {
		var high = a.high.add(b.high);
		var low = a.low.add(b.low);
		if( low.ucompare(a.low) < 0 )
			high = high.add(1.ofInt());
		return new Int64(high, low);
	}

	public static function sub( a : Int64, b : Int64 ) : Int64 {
		var high = a.high.sub(b.high);
		var low = a.low.sub(b.low);
		if( a.low.ucompare(b.low) < 0 )
			high = high.sub(1.ofInt());
		return new Int64(high, low);
	}

	public static function mul( a : Int64, b : Int64 ) : Int64 {
		var mask = 0xFFFF.ofInt();
		var al = a.low.and(mask), ah = a.low.ushr(16);
		var bl = b.low.and(mask), bh = b.low.ushr(16);
		var p00 = al.mul(bl);
		var p10 = ah.mul(bl);
		var p01 = al.mul(bh);
		var p11 = ah.mul(bh);
		var low = p00;
		var high = p11.add(p01.ushr(16)).add(p10.ushr(16));
		p01 = p01.shl(16); low = low.add(p01); if( low.ucompare(p01) < 0 ) high = high.add(1.ofInt());
		p10 = p10.shl(16); low = low.add(p10); if( low.ucompare(p10) < 0 ) high = high.add(1.ofInt());
		high = high.add(a.low.mul(b.high));
		high = high.add(a.high.mul(b.low));
		return new Int64(high, low);
	}

	static function divMod( modulus : Int64, divisor : Int64 ) {
		var quotient = new Int64(0.ofInt(), 0.ofInt());
		var mask = new Int64(0.ofInt(), 1.ofInt());
		divisor = new Int64(divisor.high, divisor.low);
		while( !divisor.high.isNeg() ) {
			var cmp = ucompare(divisor, modulus);
			divisor.high = divisor.high.shl(1).or(divisor.low.ushr(31));
			divisor.low = divisor.low.shl(1);
			mask.high = mask.high.shl(1).or(mask.low.ushr(31));
			mask.low = mask.low.shl(1);
			if( cmp >= 0 ) break;
		}
		while( !mask.low.or(mask.high).isZero() ) {
			if( ucompare(modulus, divisor) >= 0 ) {
				quotient.high = quotient.high.or(mask.high);
				quotient.low = quotient.low.or(mask.low);
				modulus = sub(modulus, divisor);
			}
			mask.low = mask.low.ushr(1).or(mask.high.shl(31));
			mask.high = mask.high.ushr(1);

			divisor.low = divisor.low.ushr(1).or(divisor.high.shl(31));
			divisor.high = divisor.high.ushr(1);
		}
		return { quotient : quotient, modulus : modulus };
	}

	public static inline function div( a : Int64, b : Int64 ) : Int64 {
		var sign = a.high.or(b.high).isNeg();
		if( a.high.isNeg() ) a = neg(a);
		if( b.high.isNeg() ) b = neg(b);
		var q = divMod(a, b).quotient;
		return sign ? neg(q) : q;
	}

	public static inline function mod( a : Int64, b : Int64 ) : Int64 {
		var sign = a.high.or(b.high).isNeg();
		if( a.high.isNeg() ) a = neg(a);
		if( b.high.isNeg() ) b = neg(b);
		var m = divMod(a, b).modulus;
		return sign ? neg(m) : m;
	}

	public static inline function shl( a : Int64, b : Int ) : Int64 {
		return if( b & 63 == 0 ) a else if( b & 63 < 32 ) new Int64( a.high.shl(b).or(a.low.ushr(32 - (b&63))), a.low.shl(b) ) else new Int64( a.low.shl(b - 32), 0.ofInt() );
	}

	public static inline function shr( a : Int64, b : Int ) : Int64 {
		return if( b & 63 == 0 ) a else if( b & 63 < 32 ) new Int64( a.high.shr(b), a.low.ushr(b).or(a.high.shl(32 - (b&63))) ) else new Int64( a.high.shr(31), a.high.shr(b - 32) );
	}

	public static inline function ushr( a : Int64, b : Int ) : Int64 {
		return if( b & 63 == 0 ) a else if( b & 63 < 32 ) new Int64( a.high.ushr(b), a.low.ushr(b).or(a.high.shl(32 - (b&63))) ) else new Int64( 0.ofInt(), a.high.ushr(b - 32) );
	}

	public static inline function and( a : Int64, b : Int64 ) : Int64 {
		return new Int64( a.high.and(b.high), a.low.and(b.low) );
	}

	public static inline function or( a : Int64, b : Int64 ) : Int64 {
		return new Int64( a.high.or(b.high), a.low.or(b.low) );
	}

	public static inline function xor( a : Int64, b : Int64 ) : Int64 {
		return new Int64( a.high.xor(b.high), a.low.xor(b.low) );
	}

	public static inline function neg( a : Int64 ) : Int64 {
		var high = Int32.complement(a.high);
		var low = Int32.neg(a.low);
		if( low.isZero() )
			high = high.add(1.ofInt());
		return new Int64(high,low);
	}

	public static inline function isNeg( a : Int64 ) : Bool {
		return a.high.isNeg();
	}

	public static inline function isZero( a : Int64 ) : Bool {
		return a.high.or(a.low).isZero();
	}

	public static inline function compare( a : Int64, b : Int64 ) : Int {
		var v = Int32.compare(a.high,b.high);
		return if( v != 0 ) v else Int32.ucompare(a.low, b.low);
	}

	/**
		Compare two Int64 in unsigned mode.
	**/
	public static inline function ucompare( a : Int64, b : Int64 ) : Int {
		var v = Int32.ucompare(a.high,b.high);
		return if( v != 0 ) v else Int32.ucompare(a.low, b.low);
	}

	public static inline function toStr( a : Int64 ) : String {
		return a.toString();
	}

}


#end