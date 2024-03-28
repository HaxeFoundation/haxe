/*
 * Copyright (C)2005-2023 Haxe Foundation
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

package haxe.math.bigint;

import haxe.ds.Vector;
import haxe.io.Bytes;

/* Original code courtesy Chuck Batson (github.com/cbatson) */
@:noCompletion
@:noDoc
@:allow(unit)
@:allow(haxe.math.bigint)
class BigInt_ {
	//-----------------------------------------------------------------------
	// Public interface
	//-----------------------------------------------------------------------
	public inline function abs():BigInt_ {
		if (this.sign() < 0) {
			return BigInt_.negate1(this);
		}
		var r = new MutableBigInt_();
		r.copyFrom(this);
		return r;
	}

	public function gcd(b:BigInt_):BigInt_ {
		var m:BigInt_ = this.abs();
		b = b.abs();
		var t:BigInt_;
		while (!equals2Int(b, 0)) {
			t = m;
			m = b;
			b = modulus2(t, m);
		}
		return m;
	}

	/**
		Calculates the least common multiple of the specified big integer numbers.
	**/
	public function lcm(b:BigInt_):BigInt_ {
		var m:BigInt_ = this.abs();
		var n:BigInt_ = b.abs();
		return BigInt_.divide2(BigInt_.multiply2(m, n), m.gcd(n));
	}

	/**
		Returns `true` if this big integer is equivalent to 0, otherwise returns `false`.
	**/
	public inline function isZero():Bool {
		return (m_count == 1) && (m_data.get(0) == 0);
	}

	/**
		Returns `true` if this big integer is less than 0, otherwise returns `false`.
	**/
	public inline function isNegative():Bool {
		return m_data.get(m_count - 1) < 0;
	}

	public function isPositive():Bool {
		return m_data.get(m_count - 1) >= 0;
	}

	public function isOdd():Bool {
		return ((m_data.get(0) & 1) == 1);
	}

	public function isEven():Bool {
		return ((m_data.get(0) & 1) == 0);
	}

	/**
		Retrieve the sign value of this big integer; 0 if positive, -1 if negative.
	**/
	public inline function sign():Int {
		return (m_data.get(m_count - 1) >> 31 != 0) ? -1 : 0;
	}

	public function getLowestSetBit():Int {
		if (this.isZero())
			return -1;
		var result:Int = -1;
		var i:Int = 0;
		var b = m_data.get(0);
		while (b == 0) {
			i++;
			b = m_data.get(i);
		}
		result = (i << 5) + BigIntHelper.ntz(b);
		return result;
	}

	public function bitLength():Int {
		if (m_count <= 0)
			return 0;
		return (32 * m_count - BigIntHelper.nlz(m_data.get(m_count - 1) ^ sign()));
	}

	public function bitCount():Int {
		var totalBits:Int = 0;
		var x:Int32;
		for (n in 0...this.m_count) {
			x = this.m_data.get(n);
			x = x - ((x >> 1) & 0x55555555);
			x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
			x = (x + (x >> 4)) & 0x0F0F0F0F;
			x = x + (x >> 8);
			x = x + (x >> 16);
			totalBits += x & 0x0000003F;
		}
		return totalBits;
	}

	public function testBit(n:Int):Bool {
		if (n < 0)
			throw new BigIntException(BigIntError.INVALID_ARGUMENT);
		var chunk = n >> 5; // divide by 32
		if (chunk >= m_count)
			return (sign() < 0);
		return ((m_data.get(chunk) & (1 << (n & 0x1f))) != 0);
	}

	public function setBit(n:Int):BigInt_ {
		return (testBit(n)) ? this : flipBit(n);
	}

	public function clearBit(n:Int):BigInt_ {
		return (testBit(n)) ? flipBit(n) : this;
	}

	public function flipBit(n:Int):BigInt_ {
		if (n < 0)
			throw new BigIntException(BigIntError.INVALID_ARGUMENT);
		var isNegative:Bool = sign() < 0;
		var chunk = (n >> 5) + 1;
		var changeBit:Int = (n & 0x1f);
		var r:MutableBigInt_ = new MutableBigInt_();
		if (chunk > m_count) {
			r.fixedSizeCopyFrom(this, ((changeBit == 0x1f) ? (chunk + 1) : chunk), isNegative ? 0xffffffff : 0x0);
		} else {
			if (chunk == m_count && changeBit == 0x1f) {
				r.fixedSizeCopyFrom(this, chunk + 1, (isNegative ? 0xffffffff : 0));
			} else {
				r.fixedSizeCopyFrom(this, m_count, 0);
			}
		}
		#if (python || php)
		// Temp fix for issues #10995
		if ( changeBit == 31) {
			r.m_data.set(chunk - 1, r.m_data.get(chunk - 1) ^ (1 << 31));
		} else {
			r.m_data.set(chunk - 1, r.m_data.get(chunk - 1) ^ (1 << changeBit));
		}
		#else
		r.m_data.set(chunk - 1, r.m_data.get(chunk - 1) ^ (1 << changeBit));
		#end
		r.compact();
		return r;
	}

	public static inline function getPowerOfTwo(exponent:Int):BigInt_ {
		var num = BigInt_.fromInt(1);
		var r = arithmeticShiftLeft2(num, exponent);
		return r;
	}

	public function hashCode():Int {
		var hash:Int32 = 0;
		for (n in 0...this.m_count) {
			hash = 31 * hash + this.m_data.get(n);
		}
		return hash;
	}

	public function isProbablePrime(tolerance:Int):Bool {
		if (tolerance <= 0)
			return true;
		var b:BigInt_ = this.abs();
		if (equals2Int(b, 1))
			return false;
		if (equals2Int(b, 2))
			return true;
		if (b.m_data.get(0) & 1 == 0)
			return false;

		var rounds:Int = 0;
		if (b.m_count <= 4) {
			rounds = (tolerance > 64) ? 64 : tolerance;
		} else if (b.m_count < 8) {
			rounds = 32;
		} else if (b.m_count < 16) {
			rounds = 16;
		} else if (b.m_count < 24) {
			rounds = 8;
		} else if (b.m_count < 32) {
			rounds = 4;
		} else {
			rounds = 2;
		}
		rounds = (tolerance < rounds) ? tolerance : rounds;
		return b.millerRabin(rounds);
	}

	/**
		Returns the first integer greater than this BigInteger that is probably prime.
	**/
	public function nextProbablePrime():BigInt_ {
		var r = new MutableBigInt_();
		r.copyFrom(this);
		if (m_data.get(0) & 1 == 0)
			BigIntArithmetic.addInt(r, r, 1);
		do {
			BigIntArithmetic.addInt(r, r, 2);
		} while (!r.isProbablePrime(1));
		r.compact();
		return r;
	}

	/**
		Test for numeric equality between this big integer and another.
	**/
	public function equals(other:BigInt_):Bool {
		if (this.m_count != other.m_count) {
			return false;
		}
		for (n in 0...this.m_count) {
			if (this.m_data.get(n) != other.m_data.get(n)) {
				return false;
			}
		}
		return true;
	}

	/**
		Test for numeric equality between this big integer and another.
	**/
	public function equalsInt(other:Int):Bool {
		if (this.m_count != 1) {
			return false;
		}
		return m_data.get(0) == other;
	}

	public function min(other:BigInt_):BigInt_ {
		return (BigIntArithmetic.compare(this, other) < 0) ? this : other;
	}

	public function max(other:BigInt_):BigInt_ {
		return (BigIntArithmetic.compare(this, other) > 0) ? this : other;
	}

	/**
		Get the value in decimal form.
	**/
	public inline function toString():String {
		return MultiwordArithmetic.toDecimalSigned(m_data, m_count);
	}
	
	public inline function toBase(radix:Int):String 
	{
		return MultiwordArithmetic.toBaseString(m_data, m_count,radix);
	}

	/**
		Get the value in hexadecimal form.
	**/
	public function toHex():String {
		var sb = new StringBuf();
		var i:Int = m_count;
		while (--i >= 0) {
			var v = m_data.get(i);
			for (j in 0...8) {
				var c:Int = (v >> 28) & 0x0f;
				v <<= 4;
				c = (c < 10) ? (c + 48) : (c - 10 + 97);
				sb.addChar(c);
			}
		}
		return sb.toString();
	}

	/**
		Get the value as bytes, big endian order.
	**/
	public function toBytes():Bytes {
		var result = Bytes.alloc(m_count << 2);
		for (i in 0...m_count) {
			var v:Int = m_data.get(m_count - i - 1);
			result.set((i << 2) + 0, (v >> 24) & 0xff);
			result.set((i << 2) + 1, (v >> 16) & 0xff);
			result.set((i << 2) + 2, (v >> 8) & 0xff);
			result.set((i << 2) + 3, (v >> 0) & 0xff);
		}
		return result;
	}

	/**
		Get the value as a vector of Ints.

		Values go from less significant to more significant with
		increasing index in the vector.

		Returns the number of Ints required to store the value.
	**/
	public function toInts(output:Vector<Int>):Int {
		if (output != null) {
			var n:Int = (m_count > output.length) ? output.length : m_count;
			for (i in 0...n) {
				output.set(i, m_data.get(i));
			}
		}
		return m_count;
	}

	/**
		Creates a big integer with value `value`.
	**/
	public static function fromInt(value:Int):BigInt_ {
		var c = getCachedValue(value);
		if (c == null) {
			c = newFromInt(value);
		}
		return c;
	}

	/**
		Creates a big integer with the value represented by the decimal string `value`.
	**/
	public static function fromString(value:String,radix:Int=10):BigInt_ {
		var bi = new MutableBigInt_();
		bi.setFromString(value,radix);
		return bi;
	}

	/**
		Creates a big integer with the signed value represented by
		the hexadecimal string `value`.
	**/
	public static function fromHexSigned(value:String):BigInt_ {
		var bi = new MutableBigInt_();
		bi.setFromHexSigned(value);
		return bi;
	}

	/**
		Creates a big integer with the unsigned value represented by
		the hexadecimal string `value`.
	**/
	public static function fromHexUnsigned(value:String):BigInt_ {
		var bi = new MutableBigInt_();
		bi.setFromHexUnsigned(value);
		return bi;
	}

	/**
		Creates a big integer with the signed value represented by bytes
	**/
	public static function fromBytes(value:Bytes, offset:Int = 0, length:Int = 0):BigInt_ {
		var bi = new MutableBigInt_();
		bi.setFromBigEndianBytesSigned(value, offset, length);
		return bi;
	}

	/**
		Creates a big integer with the value represented by the integer vector `value`.
	**/
	public static function fromUnsignedInts(value:Vector<Int>, length:Int = 0):BigInt_ {
		var bi = new MutableBigInt_();
		bi.setFromUnsignedInts(value, length);
		return bi;
	}

	public function square():BigInt_ {
		var r:MutableBigInt_ = new MutableBigInt_();
		BigIntArithmetic.multiply(r, this, this);
		return r;
	}

	public function modPow(exponent:BigInt_, modulus:BigInt_):BigInt_ {
		if (BigIntArithmetic.compareInt(modulus, 0) < 0)
			throw BigIntError.NEGATIVE_MODULUS;
		if (BigIntArithmetic.compareInt(modulus, 1) == 0)
			return BigInt.fromInt(0);
		if (BigIntArithmetic.compareInt(exponent, 0) == 0)
			return BigInt.fromInt(1);
		if (this.isZero())
			return BigInt.fromInt(0);
		var negExponent:Bool = (BigIntArithmetic.compareInt(exponent, 0) < 0);
		if (negExponent)
			exponent = BigInt_.negate1(exponent);
		var result:BigInt_ = modulus2(this, modulus);
		if (BigIntArithmetic.compareInt(exponent, 1) != 0) {
			if ((modulus.m_data.get(0) & 1) == 0) {
				result = modPowBarrett(result, exponent, modulus);
			} else {
				result = modPowMonty(result, exponent, modulus, true);
			}
		}
		if (negExponent) {
			result = result.modInverse(modulus);
		}
		return result;
	}

	public function pow(exponent:UInt):BigInt_ {
		if (exponent < 0)
			throw new BigIntException(BigIntError.NEGATIVE_EXPONENT);
		if (this.isZero())
			return (exponent == 0 ? BigInt.fromInt(1) : this);
		var r = BigInt_.newFromInt(1);
		var p:BigInt_ = this;
		while (true) {
			if ((exponent & 1) == 1)
				r = multiply2(p, r);
			exponent = exponent >> 1;
			if (exponent == 0)
				break;
			p = multiply2(p, p);
		}
		return r;
	}

	/* hac 14.61, pp. 608 */
	public function modInverse(modulus:BigInt_):BigInt_ {
		#if (lua)
		trace("moduluse: "+toString1(modulus,10));
		trace("this: "+toString1(this,10));
		#end
		if (modulus.sign() == -1 || modulus.isZero())
			throw new BigIntException(BigIntError.NEGATIVE_MODULUS);
		if (equals2Int(modulus, 1))
			return BigInt.ZERO;

		var isModulusEven:Bool = (BigIntArithmetic.bitwiseAndInt(modulus, 1) == 0);
		if ((BigIntArithmetic.bitwiseAndInt(this, 1) == 0) && isModulusEven || modulus.isZero())
			return BigInt.ZERO;

		var x:BigInt_ = this.abs();
		var y:BigInt_ = MutableBigInt_.fromBigInt(modulus);

		if (x.sign() == -1 || BigIntArithmetic.compare(x, modulus) >= 0)
			x = modulus2(x, modulus);
		if (equals2Int(x, 1))
			return BigInt.ONE;

		if ((BigIntArithmetic.bitwiseAndInt(x, 1) == 0) && (BigIntArithmetic.bitwiseAndInt(y, 1) == 0))
			throw new BigIntException(BigIntError.EVEN_VALUES);

		if (!isModulusEven) {
			// fast odd calculation
			#if (lua)
			trace("odd calculation");
			#end
			return modInverseOdd(x, y);
		}

		var a:BigInt_ = fromInt(1);
		var b:BigInt_ = fromInt(0);
		var c:BigInt_ = fromInt(0);
		var d:BigInt_ = fromInt(1);
		var u:BigInt_ = MutableBigInt_.fromBigInt(x);
		var v:BigInt_ = MutableBigInt_.fromBigInt(y);

		do {
			while ((BigIntArithmetic.bitwiseAndInt(u, 1) == 0)) {
				u = arithmeticShiftRight2(u, 1);
				if ((BigIntArithmetic.bitwiseAndInt(a, 1) == 1) || (BigIntArithmetic.bitwiseAndInt(b, 1) == 1)) {
					a = add2(a, y);
					b = sub2(b, x);
				}
				a = arithmeticShiftRight2(a, 1);
				b = arithmeticShiftRight2(b, 1);
			}

			while ((BigIntArithmetic.bitwiseAndInt(v, 1) == 0)) {
				v = arithmeticShiftRight2(v, 1);
				if ((BigIntArithmetic.bitwiseAndInt(c, 1) == 1) || (BigIntArithmetic.bitwiseAndInt(d, 1) == 1)) {
					c = add2(c, y);
					d = sub2(d, x);
				}

				c = arithmeticShiftRight2(c, 1);
				d = arithmeticShiftRight2(d, 1);
			}

			if (BigIntArithmetic.compare(u, v) >= 0) {
				u = sub2(u, v);
				a = sub2(a, c);
				b = sub2(b, d);
			} else {
				v = sub2(v, u);
				c = sub2(c, a);
				d = sub2(d, b);
			}
		} while (!u.isZero());

		if (!equals2Int(v, 1)) {
			return BigInt.ZERO;
		}

		while (BigIntArithmetic.compareInt(c, 0) < 0) {
			c = add2(c, modulus);
		}

		while (BigIntArithmetic.compare(c, modulus) >= 0) {
			c = sub2(c, modulus);
		}

		if (this.sign() < 0) {
			c = sub2(modulus, c);
		}

		return c;
	}

	public static function randomPrime(bits:Int32, tolerance:Int):BigInt_ {
		if (bits < 2)
			throw new BigIntException(BigIntError.INVALID_ARGUMENT);
		if (bits == 2)
			return ((Math.random() < 0.5) ? BigInt.TWO : BigInt.fromInt(3));
		var r = new MutableBigInt_();
		do {
			var bytes = randomBytes(bits);
			var excessBits = 8 * bytes.length - bits;
			bytes.set(0, bytes.get(0) | (1 << (7 - excessBits)));
			bytes.set(bytes.length - 1, bytes.get(bytes.length - 1) | 1);
			r.setFromBigEndianBytesSigned(bytes);
			if (bits > 10) {
				while (!equals2Int(r.gcd(BigInt.SMALL_PRIMES_PRODUCT), 1)) {
					BigIntArithmetic.addInt(r, r, 2);
				}
			}
		} while (!r.isProbablePrime(tolerance));
		if (r.sign() < 0)
			BigIntArithmetic.negate(r, r);
		return r;
	}

	public static function randomInRange(min:BigInt_, max:BigInt_):BigInt_ {
		min = min.abs();
		max = max.abs();
		var initCheck = BigIntArithmetic.compare(min, max);
		if (initCheck == 0)
			return min;
		if (initCheck > 0)
			throw new BigIntException(BigIntError.INVALID_ARGUMENT);
		if (min.bitLength() > (max.bitLength() >> 1))
			return add2(randomInRange(BigInt.ZERO, sub2(max, min)), min);
		for (i in 0...1000) {
			var rnd = random(max.bitLength());
			if (BigIntArithmetic.compare(rnd, min) >= 0 && BigIntArithmetic.compare(rnd, max) <= 0) {
				return rnd;
			}
		}
		return add2(random(sub2(max, min).bitLength() - 1), min);
	}

	public static function random(bits:Int32):BigInt_ {
		if (bits <= 0)
			return BigInt.ZERO;
		var r = new MutableBigInt_();
		r.setFromBigEndianBytesSigned(randomBytes(bits));
		r.compact();
		return r;
	}

	public static function divMod(dividend:BigInt_, divisor:BigInt_):{quotient:BigInt_, remainder:BigInt_} {
		var q = new MutableBigInt_();
		var r = new MutableBigInt_();
		BigIntArithmetic.divide(dividend, divisor, q, r);
		return {quotient: q, remainder: r};
	}

	//-----------------------------------------------------------------------
	// Private implementation
	//-----------------------------------------------------------------------

	private inline function getUnsignedDigitCount():Int {
		if ((m_count > 1) && (m_data.get(m_count - 1) == 0)) {
			return m_count - 1;
		}
		return m_count;
	}

	private inline function getShort(n:Int):Int {
		return MultiwordArithmetic.getShort(m_data, n);
	}

	private function compact():Void {
		if (isNegative()) {
			while (m_count > 1) {
				if ((m_data.get(m_count - 1) == -1) && (m_data.get(m_count - 2) < 0)) {
					--m_count;
				} else {
					break;
				}
			}
		} else {
			while (m_count > 1) {
				if ((m_data.get(m_count - 1) == 0) && (m_data.get(m_count - 2) >= 0)) {
					--m_count;
				} else {
					break;
				}
			}
		}
	}

	private function millerRabin(rounds:Int):Bool {
		var numLists:Int = ((this.bitLength() - 1) < s_primeNumbers.length) ? (this.bitLength() - 1) : s_primeNumbers.length;
		for (i in 0...numLists) {
			var t:Int32 = divMod(this, BigInt_.fromInt(s_primeProduct[i])).remainder.m_data.get(0);
			var primeNumbers = s_primeNumbers[i];
			for (j in 0...primeNumbers.length) {
				var prime:Int = primeNumbers[j];
				var qRem:Int = t % prime;
				if (qRem == 0) {
					return (this.bitLength() < 16 && this.m_data.get(0) == prime);
				}
			}
		}
		
		var m = subInt2(this, 1);
		var lsb = m.getLowestSetBit();
		if (lsb <= 0)
			return false;
		m = arithmeticShiftRight2(m, lsb);
		var montyRadix:BigInt_ = divMod(arithmeticShiftLeft2(BigInt.ONE, 32 * this.m_count), this).remainder;
		var minusMontyRadix:BigInt_ = sub2(this, montyRadix);
		var num:BigInt_;
		do {
			do {
				num = random(this.bitLength());
			} while (BigIntArithmetic.compare(num, BigInt.ZERO) == 0
				|| BigIntArithmetic.compare(num, montyRadix) == 0
				|| BigIntArithmetic.compare(num, minusMontyRadix) == 0
				|| BigIntArithmetic.compare(num, this) >= 0);
			var y = modPowMonty(num, m, this, false);
			if (BigIntArithmetic.compare(y, montyRadix) != 0) {
				var j:Int = 1;
				while (BigIntArithmetic.compare(y, minusMontyRadix) != 0) {
					if (j == lsb)
						return false;
					y = modPowMonty(y, BigInt.TWO, this, false);
					if (BigIntArithmetic.compare(y, montyRadix) == 0)
						return false;
					j++;
				}
			}
			rounds -= 2;
		} while (rounds >= 0);
		return true;
	}

	/* hac 14.64, pp. 610 */
	private function modInverseOdd(y:BigInt_, x:BigInt_):BigInt_ {
		var b:BigInt_ = fromInt(0);
		var d:BigInt_ = fromInt(1);
		var u:BigInt_ = MutableBigInt_.fromBigInt(x);
		var v:BigInt_ = MutableBigInt_.fromBigInt(y);
		do {
			while ((BigIntArithmetic.bitwiseAndInt(u, 1) == 0)) {
				#if (lua)
				trace("u: "+toString1(u,10));
				#end
				u = arithmeticShiftRight2(u, 1);
				if (BigIntArithmetic.bitwiseAndInt(b, 1) == 1)
					b = sub2(b, x);
				b = arithmeticShiftRight2(b, 1);
				#if (lua)
				trace("b: "+toString1(b,10));
				#end
			}
			while ((BigIntArithmetic.bitwiseAndInt(v, 1) == 0)) {
				#if (lua)
				trace("v: "+toString1(v,10));
				#end
				v = arithmeticShiftRight2(v, 1);
				if (BigIntArithmetic.bitwiseAndInt(d, 1) == 1)
					d = sub2(d, x);

				d = arithmeticShiftRight2(d, 1);
				#if (lua)
				trace("d: "+toString1(d,10));
				#end
			}
			if (BigIntArithmetic.compare(u, v) >= 0) {
				u = sub2(u, v);
				b = sub2(b, d);
			} else {
				v = sub2(v, u);
				d = sub2(d, b);
			}
		} while (!u.isZero());

		if (!equals2Int(v, 1)) {
			return BigInt.ZERO;
		}

		while (BigIntArithmetic.compareInt(d, 0) < 0) {
			d = add2(d, x);
		}

		while (BigIntArithmetic.compare(d, x) >= 0) {
			d = sub2(d, x);
		}

		return d;
	}

	private function modPowMonty(b:BigInt_, _e:BigInt_, _m:BigInt_, convert:Bool):BigInt_ {
		var n:Int,
			powR:Int,
			extraBits:Int,
			expLength:Int,
			numPowers:Int32,
			i:Int,
			window:Int32,
			mult:Int,
			lastZeroes:Int,
			windowPos:Int,
			bits:Int,
			j:Int;
		var smallMontyModulus:Bool;
		var mDash:Int32;
		var yAccum:Vector<Int32>,
			zVal:Vector<Int32>,
			tmp:Vector<Int32>,
			zSquared:Vector<Int32>,
			windowList:Vector<Int32>,
			yVal:Vector<Int32>;
		var oddPowers:Vector<Vector<Int32>>;
		var m:BigInt_ = _m, e:BigInt_ = _e;
		n = m.m_count;
		powR = 32 * n;
		smallMontyModulus = (m.bitLength() + 2) <= powR;
		mDash = m.getMQuote();
		if (convert) {
			b = divMod(BigInt_.arithmeticShiftLeft2(b, powR), m).remainder;
		}
		yAccum = new Vector<Int32>(n + 1);
		zVal = b.m_data;
		var zLen = b.m_count;
		if (zLen < n) {
			tmp = new Vector<Int32>(n);
			Vector.blit(zVal, 0, tmp, n - zLen, zLen);
			zVal = tmp;
		}
		extraBits = 0;
		if (e.m_count > 1 || e.bitCount() > 2) {
			expLength = e.bitLength();
			while (expLength > expWindowThresholds[extraBits])
				extraBits++;
		}
		numPowers = 1 << extraBits;
		oddPowers = new Vector<Vector<Int32>>(numPowers);
		oddPowers[0] = zVal;
		zSquared = zVal.copy();
		squareMonty(yAccum, zSquared, m.m_data, m.m_count, mDash, smallMontyModulus);
		for (i in 1...numPowers) {
			oddPowers[i] = oddPowers[i - 1].copy();
			multiplyMonty(yAccum, oddPowers[i], zSquared, m.m_data, m.m_count, mDash, smallMontyModulus);
		}
		windowList = getWindowList(e.m_data, e.m_count, extraBits);
		window = windowList[0];
		mult = window & 0xFF;
		lastZeroes = window >> 8;
		if (mult == 1) {
			yVal = zSquared;
			lastZeroes--;
		} else {
			yVal = oddPowers[mult >> 1].copy();
		}
		windowPos = 1;
		window = windowList[windowPos];
		windowPos++;
		while (window != -1) {
			mult = window & 0xFF;
			bits = lastZeroes + BitLengthTable[mult];
			j = 0;
			while (j < bits) {
				squareMonty(yAccum, yVal, m.m_data, m.m_count, mDash, smallMontyModulus);
				j++;
			}
			multiplyMonty(yAccum, yVal, oddPowers[mult >> 1], m.m_data, m.m_count, mDash, smallMontyModulus);
			lastZeroes = window >> 8;
			window = windowList[windowPos];
			windowPos++;
		}
		for (i in 0...lastZeroes) {
			squareMonty(yAccum, yVal, m.m_data, m.m_count, mDash, smallMontyModulus);
		}
		if (convert) {
			montgomeryReduce(yVal, m.m_data, m.m_count, mDash);
		} else if (smallMontyModulus && compareMonty(yVal, m.m_data) >= 0) {
			subtractMonty(yVal,m.m_data);
		}
		var montResult:BigInt_ = BigInt_.fromUnsignedInts(yVal);
		return montResult;
	}

	private function squareMonty(a:Vector<Int32>, x:Vector<Int32>, m:Vector<Int32>, mLen:Int, mDash:Int32, smallMontyModulus:Bool):Void {
		var n:Int, aMax:Int, j:Int, i:Int;
		var xVal:Int, a0:Int;
		var x0:Int64, carry:Int64, t:Int64, prod1:Int64, prod2:Int64, xi:Int64, u:Int64;
		n = mLen;
		x0 = Int64.make(0, x[0]);
		carry = Int64.mul(x0, x0);
		u = Int64.make(0, Int64.mul(carry.low, mDash).low);
		prod2 = Int64.mul(u, Int64.make(0, m[0]));
		carry = Int64.add(carry, Int64.make(0, prod2.low));
		carry = Int64.add(Int64.ushr(carry, 32), Int64.ushr(prod2, 32));
		j = 1;
		while (j < mLen) {
			prod1 = Int64.mul(x0, Int64.make(0, x[j]));
			prod2 = Int64.mul(u, Int64.make(0, m[j]));
			carry = Int64.add(carry, Int64.add(Int64.make(0, Int64.shl(prod1, 1).low), Int64.make(0, prod2.low)));
			a[j - 1] = carry.low;
			carry = Int64.add(Int64.add(Int64.ushr(carry, 32), Int64.ushr(prod2, 32)), Int64.ushr(prod1, 31));
			j++;
		}
		a[mLen] = Int64.ushr(carry, 32).low;
		a[mLen - 1] = carry.low;
		i = 1;
		while (i < mLen) {
			a0 = a[0];
			u = Int64.make(0, Int64.mul(a0, mDash).low);
			carry = Int64.add(Int64.mul(u, Int64.make(0, m[0])), Int64.make(0, a0));
			carry = Int64.ushr(carry, 32);
			j = 1;
			while (j < i) {
				carry = Int64.add(carry, Int64.add(Int64.mul(u, Int64.make(0, m[j])), Int64.make(0, a[j])));
				a[j - 1] = carry.low;
				carry = Int64.ushr(carry, 32);
				j++;
			}
			xi = Int64.make(0, x[i]);
			prod1 = Int64.mul(xi, xi);
			prod2 = Int64.mul(u, Int64.make(0, m[i]));
			carry += Int64.add(Int64.add(Int64.make(0, prod1.low), Int64.make(0, prod2.low)), Int64.make(0, a[i]));
			a[i - 1] = carry.low;
			carry = Int64.add(Int64.add(Int64.ushr(carry, 32), Int64.ushr(prod1, 32)), Int64.ushr(prod2, 32));
			j = i + 1;
			while (j < n) {
				prod1 = Int64.mul(xi, Int64.make(0, x[j]));
				prod2 = Int64.mul(u, Int64.make(0, m[j]));
				carry = Int64.add(carry, Int64.add(Int64.add(Int64.make(0, Int64.shl(prod1, 1).low), Int64.make(0, prod2.low)), Int64.make(0, a[j])));
				a[j - 1] = carry.low;
				carry = Int64.add(Int64.add(Int64.ushr(carry, 32), Int64.ushr(prod1, 31)), Int64.ushr(prod2, 32));
				j++;
			}
			carry = Int64.add(carry, Int64.make(0, a[n]));
			a[n] = Int64.ushr(carry, 32).low;
			a[n - 1] = carry.low;
			i++;
		}

		if (!smallMontyModulus && compareMonty(a, m) >= 0) {
			subtractMonty(a,m);
		}
		Vector.blit(a, 0, x, 0, n);
	}

	private function multiplyMonty(a:Vector<Int32>, x:Vector<Int32>, y:Vector<Int32>, m:Vector<Int32>, mLen:Int, mDash:Int32, smallMontyModulus:Bool):Void {
		var n:Int, aMax:Int, j:Int, i:Int;
		var a0:Int64, y0:Int64;
		var carry:Int64, t:Int64, prod1:Int64, prod2:Int64, xi:Int64, u:Int64;
		n = mLen;
		y0 = Int64.make(0, y[0]);
		i = 0;
		while (i <= n) {
			a[i] = 0;
			i++;
		}
		i = 0;
		while (i < n) {
			a0 = Int64.make(0, a[0]);
			xi = Int64.make(0, x[i]);
			prod1 = Int64.mul(xi, y0);
			carry = Int64.add(Int64.make(0, prod1.low), a0);
			u = Int64.make(0, Int64.mul(carry.low, mDash).low);
			prod2 = Int64.mul(u, Int64.make(0, m[0]));
			carry = Int64.add(carry, Int64.make(0, prod2.low));
			carry = Int64.add(Int64.add(Int64.ushr(carry, 32), Int64.ushr(prod1, 32)), Int64.ushr(prod2, 32));
			j = 1;
			while (j <= (n - 1)) {
				prod1 = Int64.mul(xi, Int64.make(0, y[j]));
				prod2 = Int64.mul(u, Int64.make(0, m[j]));
				carry = Int64.add(Int64.add(Int64.make(0, prod1.low), Int64.make(0, prod2.low)), Int64.add(Int64.make(0, a[j]), carry));
				a[j - 1] = carry.low;
				carry = Int64.add(Int64.add(Int64.ushr(carry, 32), Int64.ushr(prod1, 32)), Int64.ushr(prod2, 32));
				j++;
			}
			carry = Int64.add(carry, Int64.make(0, a[n]));
			a[n] = Int64.ushr(carry, 32).low;
			a[n - 1] = carry.low;
			i++;
		}

		if (!smallMontyModulus && compareMonty(a, m) >= 0) {
			subtractMonty(a,m);
		}
		Vector.blit(a, 0, x, 0, n);
	}

	private function montgomeryReduce(x:Vector<Int32>, m:Vector<Int32>, mLen:Int, mDash:Int32):Void {
		var n:Int, i:Int, j:Int;
		var x0:Int;
		var t:Int64, carry:Int64;
		n = mLen;
		i = 0;
		while (i < n) {
			x0 = x[0];
			t = Int64.make(0, Int64.mul(x0, mDash).low);
			carry = Int64.add(Int64.mul(t, Int64.make(0, m[0])), Int64.make(0, x0));
			carry = Int64.ushr(carry, 32);
			j = 1;
			while (j < n) {
				carry = Int64.add(carry, Int64.add(Int64.mul(t, Int64.make(0, m[j])), Int64.make(0, x[j])));
				x[j - 1] = carry.low;
				carry = Int64.ushr(carry, 32);
				j++;
			}
			x[n - 1] = carry.low;
			i++;
		}
		if (compareMonty(x, m) >= 0) {
			subtractMonty(x,m);
		}
	}
	
	// x = x - y - where x is >= y
	private function subtractMonty(x:Vector<Int32>,y:Vector<Int32>):Void {
		var yIndex:Int = y.length-1;
		while(yIndex>=0 && y[yIndex]==0) {
			yIndex--;
		}
		var xn : Int, yn : Int, i:Int = 0;
		var c : Int = 0, z : Int = 0;
		while(i<=yIndex)
		{
			xn = x.get(i);
			yn = y.get(i);
			z = xn - yn - c;
			x.set(i, z);
			c = ((~xn & yn) | (~(xn ^ yn) & z)) >>> 31;
			i++;
		}
	}

	private function compareMonty(x:Vector<Int32>, y:Vector<Int32>):Int {
		var xIndex:Int = 0;
		var yIndex:Int = 0;
		var xLen:Int = x.length - 1;
		var yLen:Int = y.length - 1;
		while (xIndex != x.length && x[xLen - xIndex] == 0) {
			xIndex++;
		}
		while (yIndex != y.length && y[yLen - yIndex] == 0) {
			yIndex++;
		}
		var diff:Int = (x.length - y.length) - (xIndex - yIndex);
		if (diff != 0) {
			return diff < 0 ? -1 : 1;
		}
		var xn:Int, yn:Int;
		while (xIndex >= 0) {
			xn = x[xLen - xIndex];
			xIndex--;
			yn = y[yLen - yIndex];
			yIndex--;
			if (xn != yn) {
				return (xn ^ -2147483648) < (yn ^ -2147483648) ? -1 : 1;
			}
		}
		return 0;
	}

	private function getMQuote():Int32 {
		var d:Int = -m_data[0];
		var mQuote:Int32 = modInverse32(d);
		return mQuote;
	}

	private function modInverse32(d:Int):Int32 {
		var x:Int32;
		x = d + (((d + 1) & 4) << 1);
		x = x * (2 - (d * x));
		x = x * (2 - (d * x));
		x = x * (2 - (d * x));
		return x;
	}

	private function modPowBarrett(base:BigInt_, exponent:BigInt_, modulus:BigInt_):BigInt_ {
		var i:Int, j:Int, k:Int;
		var extraBits:Int,
			expLength:Int,
			numPowers:Int32,
			window:Int32,
			mult:Int,
			lastZeroes:Int,
			windowPos:Int,
			bits:Int;
		var mr:BigInt_, yu:BigInt_, b2:BigInt_, y:BigInt_;
		var e:BigInt_ = exponent;
		var m:BigInt_ = modulus;
		var oddPowers:Vector<BigInt_>;
		var windowList:Vector<Int32>;

		k = m.m_count;
		mr = BigInt_.arithmeticShiftLeft2(BigInt.ONE, (k + 1) << 5);
		yu = BigInt_.divide2(BigInt_.arithmeticShiftLeft2(BigInt.ONE, k << 6), m);
		extraBits = 0;
		expLength = e.bitLength();
		while (expLength > expWindowThresholds[extraBits])
			extraBits++;
		numPowers = 1 << extraBits;
		oddPowers = new Vector<BigInt_>(numPowers);
		oddPowers[0] = base;
		b2 = reduceBarrett(base.square(), m, mr, yu);
		for (i in 1...numPowers) {
			oddPowers[i] = reduceBarrett(multiply2(oddPowers[i - 1], b2), m, mr, yu);
		}

		windowList = getWindowList(e.m_data, e.m_count, extraBits);
		window = windowList[0];
		mult = window & 0xFF;
		lastZeroes = window >> 8;
		if (mult == 1) {
			y = b2;
			lastZeroes--;
		} else {
			y = oddPowers[mult >> 1];
		}

		windowPos = 1;
		window = windowList[windowPos];
		windowPos++;
		while (window != -1) {
			mult = window & 0xFF;
			bits = lastZeroes + BitLengthTable[mult];
			j = 0;
			while (j < bits) {
				y = reduceBarrett(y.square(), m, mr, yu);
				j++;
			}
			y = reduceBarrett(multiply2(y, (oddPowers[mult >> 1])), m, mr, yu);
			lastZeroes = window >> 8;
			window = windowList[windowPos];
			windowPos++;
		}
		i = 0;
		while (i < lastZeroes) {
			y = reduceBarrett(y.square(), m, mr, yu);
			i++;
		}

		return y;
	}

	private function getWindowList(mag:Vector<Int32>, magLen:Int, extraBits:Int):Vector<Int32> {
		var i:Int,
			v:Int32,
			leadingBits:Int,
			resultSize:Int,
			resultPos:Int,
			bitPos:Int,
			mult:Int32,
			multLimit:Int32,
			zeroes:Int32;
		v = mag[magLen - 1];
		leadingBits = BigIntHelper.bitLen(v);
		resultSize = Math.floor((((magLen - 1) << 5) + leadingBits) / (1 + extraBits) + 2);
		var result:Vector<Int32> = new Vector<Int32>(resultSize);
		resultPos = 0;
		bitPos = 33 - leadingBits;
		v = v << bitPos;
		mult = 1;
		multLimit = 1 << extraBits;
		zeroes = 0;
		i = magLen - 1;
		while (i >= 0) {
			while (bitPos < 32) {
				if (mult < multLimit) {
					mult = (mult << 1) | (v >>> 31);
				} else if (v < 0) {
					result[resultPos] = createWindowEntry(mult, zeroes);
					resultPos++;
					mult = 1;
					zeroes = 0;
				} else
					zeroes++;

				v = v << 1;
				bitPos++;
			}
			i--;
			if (i < 0) {
				result[resultPos] = createWindowEntry(mult, zeroes);
				resultPos++;
				break;
			}
			v = mag[i];
			bitPos = 0;
		}
		result[resultPos] = -1;
		return result;
	}

	private function createWindowEntry(mult:Int32, zeroes:Int32):Int32 {
		while ((mult & 1) == 0) {
			mult >>>= 1;
			++zeroes;
		}
		return mult | (zeroes << 8);
	}

	private function reduceBarrett(x:BigInt_, m:BigInt_, mr:BigInt_, yu:BigInt_):BigInt_ {
		var timestamp:Float = Timer.stamp();
		var xLen:Int, mLen:Int, k:Int;
		var q1:BigInt_, q2:BigInt_, q3:BigInt_, r1:BigInt_, r2:BigInt_, r3:BigInt_;
		xLen = x.bitLength();
		mLen = m.bitLength();
		if (xLen < mLen) {
			return x;
		}
		if ((xLen - mLen) > 1) {
			var k:Int = m.m_count;
			var q1:BigInt_ = x.divideWords(k - 1);
			var q2:BigInt_ = multiply2(yu, q1);
			var q3:BigInt_ = q2.divideWords(k + 1);
			var r1:BigInt_ = x.remainderWords(k + 1);
			var r2:BigInt_ = multiply2(q3, m);
			var r3:BigInt_ = r2.remainderWords(k + 1);
			x = sub2(r1, r3);
			if (x.sign() < 0) {
				x = add2(x, mr);
			}
		}

		while (BigIntArithmetic.compare(x, m) >= 0) {
			x = sub2(x, m);
		}
		return x;
	}

	private function divideWords(w:Int):BigInt_ {
		var n:Int = m_count;
		if (w >= n)
			return BigInt.ZERO;
		var bi = new MutableBigInt_();
		bi.setFromVector(m_data, m_count - (n - w), n - w);
		return bi;
	}

	private function remainderWords(w:Int):BigInt_ {
		var n:Int = m_count;
		if (w >= n)
			return this;
		var bi = new MutableBigInt_();
		bi.setFromVector(m_data, 0, w);
		return bi;
	}

	private static function newFromInt(value:Int):BigInt_ {
		var bi = new MutableBigInt_();
		bi.setFromInt(value);
		return bi;
	}

	private static function randomBytes(bits:Int32):Bytes {
		var countBytes:Int = Std.int((bits + 7) / 8);
		var randomBytes = Bytes.alloc(countBytes);
		for (j in 0...countBytes) {
			var rndN = Math.floor(Math.random() * 256);
			randomBytes.set(j, rndN);
		}
		var excessBits:Int = 8 * countBytes - bits;
		if (excessBits > 0)
			randomBytes.set(0, randomBytes.get(0) & (255 >>> excessBits));
		return randomBytes;
	}

	private function new():Void {}

	private static function getCachedValue(value:Int):BigInt_ {
		if ((s_firstCachedValue <= value) && (value <= s_lastCachedValue)) {
			initCache();
			return s_cache[value - s_firstCachedValue];
		}
		return null;
	}

	private static function initCache():Void {
		if (s_cache == null) {
			s_cache = new Vector<BigInt_>(s_lastCachedValue + 1 - s_firstCachedValue);
			for (i in 0...s_cache.length) {
				s_cache[i] = newFromInt(i + s_firstCachedValue);
			}
		}
	}

	private var m_count:Int;
	private var m_data:Vector<Int>;

	private static inline var s_firstCachedValue:Int = -16;
	private static inline var s_lastCachedValue:Int = 16;
	private static var s_cache:Vector<BigInt_> = null;

	//-----------------------------------------------------------------------
	// Static helpers
	//-----------------------------------------------------------------------

	@:noCompletion
	private static inline function negate1(a:BigInt_):BigInt_ {
		var r = new MutableBigInt_();
		BigIntArithmetic.negate(r, a);
		return r;
	}

	@:noCompletion
	private static inline function equals2Int(a:BigInt_, b:Int):Bool {
		return a.equalsInt(b);
	}

	@:noCompletion
	private static inline function equals2(a:BigInt_, b:BigInt_):Bool {
		return a.equals(b);
	}

	@:noCompletion
	private static inline function addInt2(a:BigInt_, b:Int):BigInt_ {
		var r = new MutableBigInt_();
		BigIntArithmetic.addInt(r, a, b);
		return r;
	}

	@:noCompletion
	private static inline function add2(a:BigInt_, b:BigInt_):BigInt_ {
		var r = new MutableBigInt_();
		BigIntArithmetic.add(r, a, b);
		return r;
	}

	@:noCompletion
	private static inline function subInt2(a:BigInt_, b:Int):BigInt_ {
		var r = new MutableBigInt_();
		BigIntArithmetic.subtractInt(r, a, b);
		return r;
	}

	@:noCompletion
	private static inline function sub2(a:BigInt_, b:BigInt_):BigInt_ {
		var r = new MutableBigInt_();
		BigIntArithmetic.subtract(r, a, b);
		return r;
	}

	@:noCompletion
	private static inline function multiplyInt2(a:BigInt_, b:Int):BigInt_ {
		var r = new MutableBigInt_();
		BigIntArithmetic.multiplyInt(r, a, b);
		return r;
	}

	@:noCompletion
	private static inline function multiply2(a:BigInt_, b:BigInt_):BigInt_ {
		var r = new MutableBigInt_();
		BigIntArithmetic.multiply(r, a, b);
		return r;
	}

	@:noCompletion
	private static inline function divideInt2(a:BigInt_, b:Int):BigInt_ {
		var q = new MutableBigInt_();
		BigIntArithmetic.divideInt(a, b, q);
		return q;
	}

	@:noCompletion
	private static inline function divide2(a:BigInt_, b:BigInt_):BigInt_ {
		var q = new MutableBigInt_();
		BigIntArithmetic.divide(a, b, q, null);
		return q;
	}

	@:noCompletion
	private static inline function modulusInt2(a:BigInt_, b:Int):Int {
		var q = new MutableBigInt_();
		return BigIntArithmetic.divideInt(a, b, q);
	}

	@:noCompletion
	private static inline function modulus2(a:BigInt_, b:BigInt_):BigInt_ {
		var q = new MutableBigInt_();
		var r = new MutableBigInt_();
		BigIntArithmetic.divide(a, b, q, r);
		return r;
	}

	@:noCompletion
	private static inline function arithmeticShiftLeft2(a:BigInt_, b:Int):BigInt_ {
		var r = new MutableBigInt_();
		BigIntArithmetic.arithmeticShiftLeft(r, a, b);
		return r;
	}

	@:noCompletion
	private static inline function arithmeticShiftRight2(a:BigInt_, b:Int):BigInt_ {
		var r = new MutableBigInt_();
		BigIntArithmetic.arithmeticShiftRight(r, a, b);
		return r;
	}

	@:noCompletion
	private static inline function sign1(a:BigInt_):Int {
		return a.sign();
	}

	@:noCompletion
	private static inline function isZero1(a:BigInt_):Bool {
		return a.isZero();
	}

	@:noCompletion
	private static inline function isNegative1(a:BigInt_):Bool {
		return a.isNegative();
	}

	@:noCompletion
	private static inline function isPositive1(a:BigInt_):Bool {
		return a.isPositive();
	}

	@:noCompletion
	private static inline function isOdd1(a:BigInt_):Bool {
		return a.isOdd();
	}

	@:noCompletion
	private static inline function isEven1(a:BigInt_):Bool {
		return a.isEven();
	}

	@:noCompletion
	private static inline function toString1(a:BigInt_, radix:Int):String {
		if ((radix == 10 ) || ( radix <2 || radix >36 )) return a.toString();
		if (radix == 16 ) return a.toHex();
		return a.toBase(radix);
	}

	@:noCompletion
	private static inline function toHex1(a:BigInt_):String {
		return a.toHex();
	}

	@:noCompletion
	private static inline function toBytes1(a:BigInt_):Bytes {
		return a.toBytes();
	}

	@:noCompletion
	private static inline function toInts1(a:BigInt_, v:Vector<Int>):Int {
		return a.toInts(v);
	}

	static final BitLengthTable:Array<Int> = [
		0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
		6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
		7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
		8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
		8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
		8, 8, 8, 8, 8, 8
	];

	var expWindowThresholds:Vector<Int> = Vector.fromArrayCopy([7, 25, 81, 241, 673, 1793, 4609, 2147483647]);
	
	private static final s_primeProduct:Array<Int> = [
		111546435, 58642669, 600662303, 33984931, 89809099, 167375713, 371700317, 645328247, 1070560157, 1596463769, 11592209, 13420567, 16965341, 20193023,
		23300239, 29884301, 35360399, 42749359, 49143869, 56466073, 65111573, 76027969, 84208541, 94593973, 103569859, 119319383, 133390067, 154769821,
		178433279, 193397129, 213479407, 229580147, 250367549, 271661713, 293158127, 319512181, 357349471, 393806449, 422400701, 452366557, 507436351,
		547978913, 575204137, 627947039, 666785731, 710381447, 777767161, 834985999, 894826021, 951747481, 1019050649, 1072651369, 1125878063, 1185362993,
		1267745273, 1322520163, 1391119619, 1498299287, 1608372013, 1700725291, 1805418283, 1871456063, 2008071007, 2115193573, -2116537769, -2048682597,
		-1909179209, -1702980825, -1489962503, -1372818057, -1273647213, -1176554679, -1029034995, -962574873, -771748953, -583131125, -457088133, -303174767,
		-155320833, -61811709, 118284847, 331869553, 586295383, 772701351, 975998995, 1184460183, 1405182073, 1650949557, 1998668581, -2047251531,
		-1927652605, -1654913601, -1315052365, -972124375, -681632865, -537896625, -294451281, 45921629, 378607295, 561165965, 906561455, 1160065935,
		1380657927, 2004409167, -1889883587, -1580206561, -1192992209, -923130317, -627882485, -110329421, 120545641, 534097439, 738416003, 1094999525,
		1528839403, 1959463903, -1847439821, -979396295, -644014227, -93511911, 524494345, 1087323987, 1634127285, 2004297177, -2016987781, -1667747257,
		-1343966817, -896726901, -320897353, 297288423, 657704777, 1374982823, 1828886847, -1690887095, -1136243417, -366760067, 243359903, 969193253,
		1709898031, -1905679739
	];
	
	private static final s_primeNumbers:Array<Array<Int>> = [
		[3, 5, 7, 11, 13, 17, 19, 23], [29, 31, 37, 41, 43],[47, 53, 59, 61, 67],[71, 73, 79, 83],
		[89, 97, 101, 103],[107, 109, 113, 127],[131, 137, 139, 149],[151, 157, 163, 167],
		[173, 179, 181, 191],[193, 197, 199, 211],[223, 227, 229],[233, 239, 241],
		[251, 257, 263],[269, 271, 277],[281, 283, 293],[307, 311, 313],
		[317, 331, 337],[347, 349, 353],[359, 367, 373],[379, 383, 389],
		[397, 401, 409],[419, 421, 431],[433, 439, 443],[449, 457, 461],
		[463, 467, 479],[487, 491, 499],[503, 509, 521],[523, 541, 547],
		[557, 563, 569],[571, 577, 587],[593, 599, 601],[607, 613, 617],
		[619, 631, 641],[643, 647, 653],[659, 661, 673],[677, 683, 691],
		[701, 709, 719],[727, 733, 739],[743, 751, 757],[761, 769, 773],
		[787, 797, 809],[811, 821, 823],[827, 829, 839],[853, 857, 859],
		[863, 877, 881],[883, 887, 907],[911, 919, 929],[937, 941, 947],
		[953, 967, 971],[977, 983, 991],[997, 1009, 1013],[1019, 1021, 1031],
		[1033, 1039, 1049],[1051, 1061, 1063],[1069, 1087, 1091],[1093, 1097, 1103],
		[1109, 1117, 1123],[1129, 1151, 1153],[1163, 1171, 1181],[1187, 1193, 1201],
		[1213, 1217, 1223],[1229, 1231, 1237],[1249, 1259, 1277],[1279, 1283, 1289],
		[1291, 1297, 1301],[1303, 1307, 1319],[1321, 1327, 1361],[1367, 1373, 1381],
		[1399, 1409, 1423],[1427, 1429, 1433],[1439, 1447, 1451],[1453, 1459, 1471],
		[1481, 1483, 1487],[1489, 1493, 1499],[1511, 1523, 1531],[1543, 1549, 1553],
		[1559, 1567, 1571],[1579, 1583, 1597],[1601, 1607, 1609],[1613, 1619, 1621],
		[1627, 1637, 1657],[1663, 1667, 1669],[1693, 1697, 1699],[1709, 1721, 1723],
		[1733, 1741, 1747],[1753, 1759, 1777],[1783, 1787, 1789],[1801, 1811, 1823],
		[1831, 1847, 1861],[1867, 1871, 1873],[1877, 1879, 1889],[1901, 1907, 1913],
		[1931, 1933, 1949],[1951, 1973, 1979],[1987, 1993, 1997],[1999, 2003, 2011],
		[2017, 2027, 2029],[2039, 2053,	2063],[2069, 2081, 2083],[2087, 2089, 2099],
		[2111, 2113, 2129],[2131, 2137, 2141],[2143, 2153, 2161],[2179, 2203, 2207],
		[2213, 2221, 2237],[2239, 2243, 2251],[2267, 2269, 2273],[2281, 2287, 2293],
		[2297, 2309, 2311],[2333, 2339, 2341],[2347, 2351, 2357],[2371, 2377, 2381],
		[2383, 2389, 2393],[2399, 2411, 2417],[2423, 2437, 2441],[2447, 2459, 2467],
		[2473, 2477, 2503],[2521, 2531, 2539],[2543, 2549, 2551],[2557, 2579, 2591],
		[2593, 2609, 2617],[2621, 2633, 2647],[2657, 2659, 2663],[2671, 2677, 2683],
		[2687, 2689, 2693],[2699, 2707, 2711],[2713, 2719, 2729],[2731, 2741, 2749],
		[2753, 2767, 2777],[2789, 2791, 2797],[2801, 2803, 2819],[2833, 2837, 2843],
		[2851, 2857, 2861],[2879, 2887, 2897],[2903, 2909, 2917],[2927, 2939, 2953], 
		[2957, 2963, 2969],[2971, 2999, 3001],[3011, 3019, 3023],[3037, 3041, 3049]
	];
}
