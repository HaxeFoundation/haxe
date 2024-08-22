/*
 * Copyright (C)2005-2019 Haxe Foundation
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

using haxe.Int128;

/**
	A cross-platform signed 128-bit integer.
	Int128 instances can be created from two 64-bit words using `Int128.make()`.
**/
#if flash
@:notNull
#end
@:transitive
abstract Int128(__Int128) from __Int128 to __Int128 {
	private inline function new(x:__Int128)
		this = x;

	/**
		Makes a copy of `this` Int128.
	**/
	public inline function copy():Int128
		return Int128.make(high, low);

	/**
		Construct an Int128 from two 64-bit words `high` and `low`.
	**/
	public static inline function make(high:Int64, low:Int64):Int128
		return new Int128(new __Int128(high, low));

	/**
		Returns an Int128 with the value of the Int `x`.
		`x` is sign-extended to fill 64 bits.
	**/
	@:from public static inline function ofInt(x:Int):Int128
		#if lua return make((x : Int32) >> 31, (x : Int32)); #else return make(x >> 31, x); #end

	/**
		Returns an Int128 with the value of the Int64 `x`.
		`x` is sign-extended to fill 64 bits.
	**/
	@:from public static inline function ofInt64(x:Int64):Int128
		#if lua return make((x : Int64) >> 63, (x : Int64)); #else return make(x >> 63, x); #end

	/**
		Returns an Int with the value of the Int128 `x`.
		Throws an exception  if `x` cannot be represented in 32 bits.
	**/
	public static inline function toInt(x:Int128):Int {
		return Int64.toInt(x.low);
	}

	/**
		Returns an Int64 with the value of the Int128 `x`.
		Throws an exception  if `x` cannot be represented in 64 bits.
	**/
	public static inline function toInt64(x:Int128):Int64 {
		// This is a completely different overflow check because we're using Int128's.
		if (x.low == x.high << 63 && x.low.high < x.high)
			throw "Overflow";

		return x.low;
	}

	@:deprecated('haxe.Int128.is() is deprecated. Use haxe.Int128.isInt128() instead')
	inline public static function is(val:Dynamic):Bool {
		return isInt128(val);
	}

	/**
		Returns whether the value `val` is of type `haxe.Int128`
	**/
	inline public static function isInt128(val:Dynamic):Bool
		return Std.isOfType(val, __Int128);

	/**
		Returns `true` if `x` is less than zero.
	**/
	public static inline function isNeg(x:Int128):Bool
		return x.high < 0;

	/**
		Returns `true` if `x` is exactly zero.
	**/
	public static inline function isZero(x:Int128):Bool
		return x == 0;

	/**
		Compares `a` and `b` in signed mode.
		Returns a negative value if `a < b`, positive if `a > b`,
		or 0 if `a == b`.
	**/
	public static inline function compare(a:Int128, b:Int128):Int64 {
		var v = a.high - b.high;
		v = if (v != 0) v else Int64.ucompare(a.low, b.low);
		return a.high < 0 ? (b.high < 0 ? v : -1) : (b.high >= 0 ? v : 1);
	}

	/**
		Compares `a` and `b` in unsigned mode.
		Returns a negative value if `a < b`, positive if `a > b`,
		or 0 if `a == b`.
	**/
	public static inline function ucompare(a:Int128, b:Int128):Int64 {
		var v = Int64.ucompare(a.high, b.high);
		return if (v != 0) v else Int64.ucompare(a.low, b.low);
	}

	/**
		Returns a signed decimal `String` representation of `x`.
	**/
	public static inline function toStr(x:Int128):String
		return x.toString();

	function toString():String {
		var i:Int128 = cast this;
		if (i == 0)
			return "0";
		var str = "";
		var neg = false;
		if (i.isNeg()) {
			neg = true;
			// i = -i; cannot negate here as --170141183420855150465331762880109871103 = -170141183420855150465331762880109871103
		}
		var ten:Int128 = 10;
		while (i != 0) {
			var r = i.divMod(ten);
			if (r.modulus.isNeg()) {
				str = -(r.modulus).low + str;
				i = -r.quotient;
			} else {
				str = r.modulus.low + str;
				i = r.quotient;
			}
		}
		if (neg)
			str = "-" + str;
		return str;
	}

	public static inline function parseString(sParam:String):Int128 {
		return Int128Helper.parseString(sParam);
	}

	public static inline function fromFloat(f:Float):Int128 {
		return Int128Helper.fromFloat(f);
	}

	/**
		Performs signed integer divison of `dividend` by `divisor`.
		Returns `{ quotient : Int128, modulus : Int128 }`.
	**/
	public static function divMod(dividend:Int128, divisor:Int128):{quotient:Int128, modulus:Int128} {
		// Handle special cases of 0 and 1
		if (divisor.high == 0) {
			if (divisor.low == 0) throw "divide by zero";
			if (divisor.low == 1) return {quotient: dividend.copy(), modulus: 0};
		}

		var divSign = dividend.isNeg() != divisor.isNeg();

		var modulus = dividend.isNeg() ? -dividend : dividend.copy();
		divisor = divisor.isNeg() ? -divisor : divisor;

		var quotient:Int128 = 0;
		var mask:Int128 = 1;

		while (!divisor.isNeg()) {
			var cmp = ucompare(divisor, modulus);
			divisor <<= 1;
			mask <<= 1;
			if (cmp >= 0)
				break;
		}

		while (mask != 0) {
			if (ucompare(modulus, divisor) >= 0) {
				quotient |= mask;
				modulus -= divisor;
			}
			mask >>>= 1;
			divisor >>>= 1;
		}

		if (divSign)
			quotient = -quotient;
		if (dividend.isNeg())
			modulus = -modulus;

		return {
			quotient: quotient,
			modulus: modulus
		};
	}

	/**
		Returns the negative of `x`.
	**/
	@:op(-A) public static inline function neg(x:Int128):Int128 {
		var high = ~x.high;
		var low = -x.low;
		if (low == 0)
			high++;
		return make(high, low);
	}

	@:op(++A) private inline function preIncrement():Int128 {
		this = copy();
		++this.low;
		if (this.low == 0)
			++this.high;
		return cast this;
	}

	@:op(A++) private inline function postIncrement():Int128 {
		this = copy();
		this.low++;
		if (this.low == 0)
			this.high++;
		return cast this;
	}

	@:op(--A) private inline function preDecrement():Int128 {
		this = copy();
		if (this.low == 0)
			--this.high;
		--this.low;
		return cast this;
	}

	@:op(A--) private inline function postDecrement():Int128 {
		this = copy();
		if (this.low == 0)
			this.high--;
		this.low--;
		return cast this;
	}

	/**
		Returns the sum of `a` and `b`.
	**/
	@:op(A + B) public static inline function add(a:Int128, b:Int128):Int128 {
		var high = a.high + b.high;
		var low = a.low + b.low;
		if (Int64.ucompare(low, a.low) < 0)
			high++;
		return make(high, low);
	}

	@:op(A + B) @:commutative private static inline function addInt(a:Int128, b:Int):Int128
		return add(a, b);

	@:op(A + B) @:commutative private static inline function addInt64(a:Int128, b:Int64):Int128
		return add(a, b);

	/**
		Returns `a` minus `b`.
	**/
	@:op(A - B) public static inline function sub(a:Int128, b:Int128):Int128 {
		var high = a.high - b.high;
		var low = a.low - b.low;
		if (Int64.ucompare(a.low, b.low) < 0)
			high--;
		return make(high, low);
	}

	@:op(A - B) private static inline function subInt(a:Int128, b:Int):Int128
		return sub(a, b);

	@:op(A - B) private static inline function subInt64(a:Int128, b:Int64):Int128
		return sub(a, b);

	@:op(A - B) private static inline function intSub(a:Int, b:Int128):Int128
		return sub(a, b);

	@:op(A - B) private static inline function int64Sub(a:Int64, b:Int128):Int128
		return sub(a, b);

	/**
		Returns the product of `a` and `b`.
	**/
	@:op(A * B)
	public static #if !lua inline #end function mul(a:Int128, b:Int128):Int128 {
		var mask = 0xFFFFFFFF;
		var al = a.low & mask, ah = a.low >>> 32;
		var bl = b.low & mask, bh = b.low >>> 32;
		var p00 = al * bl;
		var p10 = ah * bl;
		var p01 = al * bh;
		var p11 = ah * bh;
		var low = p00;
		var high = p11 + (p01 >>> 32) + (p10 >>> 32);
		p01 <<= 16;
		low += p01;
		if (Int64.ucompare(low, p01) < 0)
			high++;
		p10 <<= 16;
		low += p10;
		if (Int64.ucompare(low, p10) < 0)
			high++;
		high += a.low * b.high + a.high * b.low;
		return make(high, low);
	}

	@:op(A * B) @:commutative private static inline function mulInt(a:Int128, b:Int):Int128
		return mul(a, b);

	@:op(A * B) @:commutative private static inline function mulInt64(a:Int128, b:Int64):Int128
		return mul(a, b);

	/**
		Returns the quotient of `a` divided by `b`.
	**/
	@:op(A / B) public static inline function div(a:Int128, b:Int128):Int128
		return divMod(a, b).quotient;

	@:op(A / B) private static inline function divInt(a:Int128, b:Int):Int128
		return div(a, b);

	@:op(A / B) private static inline function divInt64(a:Int128, b:Int64):Int128
		return div(a, b);

	@:op(A / B) private static inline function intDiv(a:Int, b:Int128):Int128
		return div(a, b).toInt();

	@:op(A / B) private static inline function intDiv64(a:Int64, b:Int128):Int128
		return div(a, b).toInt();

	/**
		Returns the modulus of `a` divided by `b`.
	**/
	@:op(A % B) public static inline function mod(a:Int128, b:Int128):Int128
		return divMod(a, b).modulus;

	@:op(A % B) private static inline function modInt(a:Int128, b:Int):Int128
		return mod(a, b).toInt();

	@:op(A % B) private static inline function modInt64(a:Int128, b:Int64):Int128
		return mod(a, b).toInt();

	@:op(A % B) private static inline function intMod(a:Int, b:Int128):Int128
		return mod(a, b).toInt();

	@:op(A % B) private static inline function intMod(a:Int64, b:Int128):Int128
		return mod(a, b).toInt();

	/**
		Returns `true` if `a` is equal to `b`.
	**/
	@:op(A == B) public static inline function eq(a:Int128, b:Int128):Bool
		return a.high == b.high && a.low == b.low;

	@:op(A == B) @:commutative private static inline function eqInt(a:Int128, b:Int):Bool
		return eq(a, b);

	@:op(A == B) @:commutative private static inline function eqInt64(a:Int128, b:Int64):Bool
		return eq(a, b);

	/**
		Returns `true` if `a` is not equal to `b`.
	**/
	@:op(A != B) public static inline function neq(a:Int128, b:Int128):Bool
		return a.high != b.high || a.low != b.low;

	@:op(A != B) @:commutative private static inline function neqInt(a:Int128, b:Int):Bool
		return neq(a, b);

	@:op(A != B) @:commutative private static inline function neqInt64(a:Int128, b:Int64):Bool
		return neq(a, b);

	@:op(A < B) private static inline function lt(a:Int128, b:Int128):Bool
		return compare(a, b) < 0;

	@:op(A < B) private static inline function ltInt(a:Int128, b:Int):Bool
		return lt(a, b);

	@:op(A < B) private static inline function intLt(a:Int, b:Int128):Bool
		return lt(a, b);

	@:op(A <= B) private static inline function lte(a:Int128, b:Int128):Bool
		return compare(a, b) <= 0;

	@:op(A <= B) private static inline function lteInt(a:Int128, b:Int):Bool
		return lte(a, b);

	@:op(A <= B) private static inline function intLte(a:Int, b:Int128):Bool
		return lte(a, b);

	@:op(A > B) private static inline function gt(a:Int128, b:Int128):Bool
		return compare(a, b) > 0;

	@:op(A > B) private static inline function gtInt(a:Int128, b:Int):Bool
		return gt(a, b);

	@:op(A > B) private static inline function intGt(a:Int, b:Int128):Bool
		return gt(a, b);

	@:op(A >= B) private static inline function gte(a:Int128, b:Int128):Bool
		return compare(a, b) >= 0;

	@:op(A >= B) private static inline function gteInt(a:Int128, b:Int):Bool
		return gte(a, b);

	@:op(A >= B) private static inline function intGte(a:Int, b:Int128):Bool
		return gte(a, b);

	/**
		Returns the bitwise NOT of `a`.
	**/
	@:op(~A) private static inline function complement(a:Int128):Int128
		return make(~a.high, ~a.low);

	/**
		Returns the bitwise AND of `a` and `b`.
	**/
	@:op(A & B) public static inline function and(a:Int128, b:Int128):Int128
		return make(a.high & b.high, a.low & b.low);

	/**
		Returns the bitwise OR of `a` and `b`.
	**/
	@:op(A | B) public static inline function or(a:Int128, b:Int128):Int128
		return make(a.high | b.high, a.low | b.low);

	/**
		Returns the bitwise XOR of `a` and `b`.
	**/
	@:op(A ^ B) public static inline function xor(a:Int128, b:Int128):Int128
		return make(a.high ^ b.high, a.low ^ b.low);

	/**
		Returns `a` left-shifted by `b` bits.
	**/
	@:op(A << B) public static inline function shl(a:Int128, b:Int):Int128 {
		b &= 127;
		return if (b == 0) a.copy() else if (b < 64) make((a.high << b) | (a.low >>> (64 - b)), a.low << b) else make(a.low << (b - 64), 0);
	}

	/**
		Returns `a` right-shifted by `b` bits in signed mode.
		`a` is sign-extended.
	**/
	@:op(A >> B) public static inline function shr(a:Int128, b:Int):Int128 {
		b &= 127;
		return if (b == 0) a.copy() else if (b < 64) make(a.high >> b, (a.high << (64 - b)) | (a.low >>> b)); else make(a.high >> 63, a.high >> (b - 64));
	}

	/**
		Returns `a` right-shifted by `b` bits in unsigned mode.
		`a` is padded with zeroes.
	**/
	@:op(A >>> B) public static inline function ushr(a:Int128, b:Int):Int128 {
		b &= 127;
		return if (b == 0) a.copy() else if (b < 64) make(a.high >>> b, (a.high << (64 - b)) | (a.low >>> b)); else make(0, a.high >>> (b - 64));
	}

	public var high(get, never):Int64;

	private inline function get_high()
		return this.high;

	private inline function set_high(x)
		return this.high = x;

	public var low(get, never):Int64;

	private inline function get_low()
		return this.low;

	private inline function set_low(x)
		return this.low = x;
}

/**
	This typedef will fool `@:coreApi` into thinking that we are using
	the same underlying type, even though it might be different on
	specific platforms.
**/
private typedef __Int128 = ___Int128;

private class ___Int128 {
	public var high:Int64;
	public var low:Int64;

	public inline function new(high, low) {
		this.high = high;
		this.low = low;
	}

	/**
		We also define toString here to ensure we always get a pretty string
		when tracing or calling `Std.string`. This tends not to happen when
		`toString` is only in the abstract.
	**/
	public function toString():String
		return Int128.toStr(cast this);
}
