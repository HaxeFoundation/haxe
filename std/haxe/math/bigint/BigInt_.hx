/*
 * Copyright (C)2005-2022 Haxe Foundation
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
@:allow(unit)
@:allow(haxe.math.bigint)
class BigInt_
{
	//-----------------------------------------------------------------------
	// Public interface
	//-----------------------------------------------------------------------

	public inline function abs():BigInt_
	{
		if ( this.sign() < 0) {
			return BigInt_.negate1(this);
		} 
		var r = new MutableBigInt_();
		r.copyFrom(this);
		return r;
	}
	
	public function gcd( b : BigInt_) : BigInt_ 
	{
		var m:BigInt_ = this.abs();
		b = b.abs();
		var t:BigInt_;
		while ( !equals2Int(b, 0) ) {
			  t = m;
			  m = b;
			  b = modulus2(t , m);
		}
		return m;
	}
	
	/**
		Calculates the least common multiple of the specified big integer numbers.
	**/
	public function lcm( b : BigInt_) : BigInt_
	{
		var m:BigInt_ = this.abs();
		var n:BigInt_ = b.abs();
		return BigInt_.divide2(BigInt_.multiply2(m,n),m.gcd(n));
	}
	
	/**
		Returns `true` if this big integer is equivalent to 0, otherwise returns `false`.
	**/
	public inline function isZero() : Bool
	{
		return (m_count == 1) && (m_data.get(0) == 0);
	}

	/**
		Returns `true` if this big integer is less than 0, otherwise returns `false`.
	**/
	public inline function isNegative() : Bool
	{
		return m_data.get(m_count - 1) < 0;
	}
	
	public function isPositive():Bool
	{
		return m_data.get(m_count - 1) >= 0;
	}

	public function isOdd():Bool
	{
		return ( ( m_data.get(0) & 1) == 1 );
	}
	
	public function isEven():Bool
	{
		return ( (m_data.get(0) & 1) == 0);
	}

	/**
		Retrieve the sign value of this big integer; 0 if positive, -1 if negative.
	**/
	public inline function sign() : Int
	{
		return ( m_data.get(m_count - 1)>>31 !=0 )?-1:0;
	}
	
	public function getLowestSetBit():Int
	{
		if ( this.isZero() ) return -1;
		var result:Int = -1;
		var i:Int = 0;
		var b = m_data.get(0);
		while ( b == 0  ) { i++; b = m_data.get(i); }
		result = (i<<5) +BigIntHelper.ntz(b);
		return result;
	}

	public function bitLength():Int {
		if ( m_count <=0) return 0;
		return ( 32 * m_count  - BigIntHelper.nlz(m_data.get(m_count-1)^sign()) );
	}
	
	public function bitCount():Int 
	{
		var totalBits:Int = 0;
		var x:Int, m:Int;
		for (n in 0...this.m_count) {
			x = this.m_data.get(n);
			m = (x >>> 1) & 0x77777777;
			x = x - m;
			m = (m >>> 1) & 0x77777777;
			x = x - m;
			m = (m >>> 1) & 0x77777777;
			x = x - m;
			x = (x + (x >>> 4)) & 0x0F0F0F0F;
			x = x * 0x01010101;
			totalBits += x >>> 24;
		}
		return totalBits;
	}
	
	public function testBit(n:Int):Bool
	{
		if ( n < 0 ) throw BigIntExceptions.INVALID_ARGUMENT;
		var chunk = n >> 5; //divide by 32
		if ( chunk >= m_count) return false;
		return ( ( m_data.get(chunk) & (1<<(n & 0x1f))) != 0);
	}
	
	public function isProbablePrime(tolerance:UInt):Bool
	{
		if ( tolerance <= 0 ) return true;
		var b:BigInt_ = this.abs();
		if ( equals2Int(b,1) ) return false;
		if ( equals2Int(b,2) ) return true;
		if ( b.m_data.get(0) & 1 == 0 ) return false;

		var rounds:UInt = 0;
		if ( b.m_count <= 4) {
			rounds = (tolerance>64)?64:tolerance;
		} else if ( b.m_count < 8) {
			rounds = 32;
		} else if ( b.m_count < 16) {
			rounds = 16;
		} else if ( b.m_count < 24) {
			rounds = 8;
		} else if ( b.m_count < 32) {
			rounds = 4;
		} else {
			rounds = 2;
		}
		rounds = (tolerance<rounds)?tolerance:rounds;
		return b.millerRabin(rounds);
	}
	
	/**
		Returns the first integer greater than this BigInteger that is probably prime.
	**/
	public function nextProbablePrime() : BigInt_
	{
		var r = new MutableBigInt_();
		r.copyFrom(this);
		if ( m_data.get(0) & 1 == 0 ) BigIntArithmetic.addInt(r, r, 1);
		do {
			BigIntArithmetic.addInt(r, r, 2);
		} while (!r.isProbablePrime(1));
		r.compact();
		return r;
	}

	/**
		Test for numeric equality between this big integer and another.
	**/
	public function equals(other : BigInt_) : Bool
	{
		if (this.m_count != other.m_count)
		{
			return false;
		}
		for (n in 0 ... this.m_count)
		{
			if (this.m_data.get(n) != other.m_data.get(n))
			{
				return false;
			}
		}
		return true;
	}

	/**
		Test for numeric equality between this big integer and another.
	**/
	public function equalsInt(other : Int) : Bool
	{
		if (this.m_count != 1)
		{
			return false;
		}
		return m_data.get(0) == other;
	}
	
	public function min(other : BigInt_ ) : BigInt_ {
		return (BigIntArithmetic.compare(this,other) < 0) ? this : other;
	}

	public function max(other : BigInt_ ) : BigInt_ {
		return (BigIntArithmetic.compare(this,other) > 0) ? this : other;
	}

	/**
		Get the value in decimal form.
	**/
	public inline function toString() : String
	{
		return MultiwordArithmetic.toDecimalSigned(m_data, m_count);
	}

	/**
		Get the value in hexadecimal form.
	**/
	public function toHex() : String
	{
		var sb = new StringBuf();
		var i : Int = m_count;
		while (--i >= 0)
		{
			var v = m_data.get(i);
			for (j in 0 ... 8)
			{
				var c : Int = (v >> 28) & 0x0f;
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
	public function toBytes() : Bytes
	{
		var result = Bytes.alloc(m_count << 2);
		for (i in 0 ... m_count)
		{
			var v : Int = m_data.get(m_count - i - 1);
			result.set((i << 2) + 0, (v >> 24) & 0xff);
			result.set((i << 2) + 1, (v >> 16) & 0xff);
			result.set((i << 2) + 2, (v >>  8) & 0xff);
			result.set((i << 2) + 3, (v >>  0) & 0xff);
		}
		return result;
	}

	/**
		Get the value as a vector of Ints.

		Values go from less significant to more significant with
		increasing index in the vector.

		Returns the number of Ints required to store the value.
	**/
	public function toInts(output : Vector<Int>) : Int
	{
		if (output != null)
		{
			var n : Int = (m_count > output.length) ? output.length : m_count;
			for (i in 0 ... n)
			{
				output.set(i, m_data.get(i));
			}
		}
		return m_count;
	}

	/**
		Creates a big integer with value `value`.
	**/
	public static function fromInt(value : Int) : BigInt_
	{
		var c = getCachedValue(value);
		if (c == null)
		{
			c = newFromInt(value);
		}
		return c;
	}

	/**
		Creates a big integer with the value represented by the decimal string `value`.
	**/
	public static function fromString(value : String) : BigInt_
	{
		var bi = new MutableBigInt_();
		bi.setFromString(value);
		return bi;
	}

	/**
		Creates a big integer with the signed value represented by
		the hexadecimal string `value`.
	**/
	public static function fromHexSigned(value : String) : BigInt_
	{
		var bi = new MutableBigInt_();
		bi.setFromHexSigned(value);
		return bi;
	}

	/**
		Creates a big integer with the unsigned value represented by
		the hexadecimal string `value`.
	**/
	public static function fromHexUnsigned(value : String) : BigInt_
	{
		var bi = new MutableBigInt_();
		bi.setFromHexUnsigned(value);
		return bi;
	}
	
	/**
		Creates a big integer with the signed value represented by bytes
	**/
	public static function fromBytes(value : Bytes, offset : Int = 0, length : Int = 0) : BigInt_
	{
		var bi = new MutableBigInt_();
		bi.setFromBigEndianBytesSigned(value, offset, length);
		return bi;
	}

	/**
		Creates a big integer with the value represented by the integer vector `value`.
	**/
	public static function fromUnsignedInts(value : Vector<Int>, length : Int = 0) : BigInt_
	{
		var bi = new MutableBigInt_();
		bi.setFromUnsignedInts(value, length);
		return bi;
	}
	
	public function modPow(exponent:BigInt_, modulus:BigInt_) : BigInt_
	{
		if (BigIntArithmetic.compareInt(exponent,0) < 0) throw BigIntExceptions.NEGATIVE_EXPONENT;
		if ( this.isZero() ) return ( BigIntArithmetic.compareInt(exponent,0) == 0 ? BigInt.fromInt(1) : this);
		var r = BigInt_.newFromInt(1);
		var p:BigInt_ = this;
		while(true) {
			if ( BigIntArithmetic.bitwiseAndInt(exponent,1) == 1 ) r = modulus2(multiply2(p,r),modulus);
			exponent= BigInt_.arithmeticShiftRight2(exponent, 1);
			if (BigIntArithmetic.compareInt(exponent,0) == 0) break;
			p = modulus2(multiply2(p,p),modulus); 
		 }
		return r;
	}
	
	public function pow(exponent:UInt) : BigInt_
	{
		if (exponent < 0) throw BigIntExceptions.NEGATIVE_EXPONENT;
		if ( this.isZero() ) return ( exponent == 0 ? BigInt.fromInt(1) : this);
		var r = BigInt_.newFromInt(1);
		var p:BigInt_ = this;
		while(true) {
			if ( (exponent & 1) == 1 ) r = multiply2(p,r);
			exponent= exponent >> 1;
			if (exponent == 0) break;
			p = multiply2(p,p); 
		 }
		return r;
	}
	
	/* hac 14.61, pp. 608 */
	public function modInverse(modulus:BigInt_):BigInt_ 
	{
		if (modulus.sign() == -1 || modulus.isZero())
			throw BigIntExceptions.NEGATIVE_MODULUS;
		if (equals2Int(modulus, 1))
			return BigInt.ZERO;

		var isModulusEven:Bool = (BigIntArithmetic.bitwiseAndInt(modulus, 1) == 0);
		if ((BigIntArithmetic.bitwiseAndInt(this, 1) == 0) && isModulusEven || modulus.isZero())
			return BigInt.ZERO;

		var x:BigInt_ = MutableBigInt_.fromBigInt(this);
		var y:BigInt_ = MutableBigInt_.fromBigInt(modulus);

		if (x.sign() == -1 || BigIntArithmetic.compare(x, modulus) >= 0)
			x = modulus2(x, modulus);
		if (equals2Int(x, 1))
			return BigInt.ONE;

		if ((BigIntArithmetic.bitwiseAndInt(x, 1) == 0) && (BigIntArithmetic.bitwiseAndInt(y, 1) == 0))
			throw BigIntExceptions.EVEN_VALUES;

		if (!isModulusEven) {
			// fast odd calculation
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

		return c;
	}
	
	public static function randomPrime(bits:Int32 , tolerance:UInt):BigInt_
	{
		if ( bits < 2 ) throw BigIntExceptions.INVALID_ARGUMENT;
		if ( bits == 2 ) return ( (Math.random()<0.5)?BigInt.TWO:BigInt.fromInt(3));
		var r = new MutableBigInt_();
		do {
			var bytes = randomBytes(bits);
			var  excessBits = 8 * bytes.length - bits;
			bytes.set(0,bytes.get(0)|(1 << (7 - excessBits)));
			bytes.set(bytes.length-1,bytes.get(bytes.length-1)|1);
			r.setFromBigEndianBytesSigned(bytes);
			if ( bits > 10 ) {
				while(!equals2Int(r.gcd(BigInt.SMALL_PRIMES_PRODUCT),1)) 
				{
					BigIntArithmetic.addInt(r, r, 2);
				}
			}
		} while (!r.isProbablePrime(tolerance));
		if ( r.sign() < 0) BigIntArithmetic.negate(r, r);
		return r;
	}
	
	public static function randomInRange(min:BigInt_, max:BigInt_):BigInt_
	{
		min = min.abs();
		max = max.abs();
		var initCheck = BigIntArithmetic.compare(min,max);
		if ( initCheck == 0) return min;
		if ( initCheck > 0 ) throw BigIntExceptions.INVALID_ARGUMENT;
		if ( min.bitLength() > (max.bitLength()>>1)) return add2(randomInRange(BigInt.ZERO,sub2(max,min)),min);
		for(i in 0...1000) {
			var rnd = random(max.bitLength());
			if ( BigIntArithmetic.compare(rnd,min) >= 0 &&  BigIntArithmetic.compare(rnd,max) <= 0) {
				return rnd;
			}
		}
		return add2(random(sub2(max,min).bitLength()-1),min);
	}
	
	public static function random(bits:Int32 ):BigInt_
	{
		if ( bits <= 0 ) return BigInt.ZERO;
		var r = new MutableBigInt_();
		r.setFromBigEndianBytesSigned(randomBytes(bits));
		r.compact();
		return r;
	}

	//-----------------------------------------------------------------------
	// Private implementation
	//-----------------------------------------------------------------------

	private inline function getUnsignedDigitCount() : Int
	{
		if ((m_count > 1) && (m_data.get(m_count - 1) == 0))
		{
			return m_count - 1;
		}
		return m_count;
	}

	private inline function getShort(n : Int) : Int
	{
		return MultiwordArithmetic.getShort(m_data, n);
	}

	private function compact() : Void
	{
		if (isNegative())
		{
			while (m_count > 1)
			{
				if ((m_data.get(m_count - 1) == -1) && (m_data.get(m_count - 2) < 0))
				{
					--m_count;
				}
				else
				{
					break;
				}
			}
		}
		else
		{
			while (m_count > 1)
			{
				if ((m_data.get(m_count - 1) == 0) && (m_data.get(m_count - 2) >= 0))
				{
					--m_count;
				}
				else
				{
					break;
				}
			}
		}
	}
	
	private function millerRabin(rounds:UInt):Bool
	{
		var minusOne:BigInt_ = subInt2(this,1);
		var m = subInt2(this,1);
		var lsb = m.getLowestSetBit();
		if ( lsb <=0 ) return false;
		m = arithmeticShiftRight2(m,lsb);
		var num:BigInt_;
		for(i in 0...rounds) {
			num = randomInRange(BigInt.TWO,minusOne);
			var z:BigInt_ = num.modPow(m,this);
			if ( BigIntArithmetic.compare(z, BigInt.ONE) != 0 && BigIntArithmetic.compare(z, minusOne) != 0) {
				var j:Int = 1;
				while ( j<=lsb  && BigIntArithmetic.compare(z, minusOne) != 0) 
				{
					if ( BigIntArithmetic.compare(z, BigInt.ONE) == 0 || j == lsb) {
					  return false;
					}
					z = z.modPow(BigInt.TWO,this);
					j++;
				}
			}
		}
		return true;
	}
	
	/* hac 14.64, pp. 610 */
	private function modInverseOdd(y:BigInt_, x:BigInt_):BigInt_ 
	{
		var b:BigInt_ = fromInt(0);
		var d:BigInt_ = fromInt(1);
		var u:BigInt_ = MutableBigInt_.fromBigInt(x);
		var v:BigInt_ = MutableBigInt_.fromBigInt(y);
		do {
			while ((BigIntArithmetic.bitwiseAndInt(u, 1) == 0)) {
				u = arithmeticShiftRight2(u, 1);
				if (BigIntArithmetic.bitwiseAndInt(b, 1) == 1)
					b = sub2(b, x);
				b = arithmeticShiftRight2(b, 1);
			}
			while ((BigIntArithmetic.bitwiseAndInt(v, 1) == 0)) {
				v = arithmeticShiftRight2(v, 1);
				if (BigIntArithmetic.bitwiseAndInt(d, 1) == 1)
					d = sub2(d, x);

				d = arithmeticShiftRight2(d, 1);
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

	private static function newFromInt(value : Int) : BigInt_
	{
		var bi = new MutableBigInt_();
		bi.setFromInt(value);
		return bi;
	}
	
	private static function randomBytes(bits:Int32) : Bytes
	{
		var countBytes:Int = Std.int((bits+7)/8);
		var randomBytes = Bytes.alloc(countBytes);
		for(j in 0...countBytes) {
			var rndN = Math.floor( Math.random() * 256 );
			randomBytes.set(j,rndN);
		}
		var excessBits:Int = 8 * countBytes - bits;
		if ( excessBits > 0)
			randomBytes.set(0, randomBytes.get(0)&(255 >>> excessBits));
		return randomBytes;
	}

	private function new() : Void
	{
	}

	private static function getCachedValue(value : Int) : BigInt_
	{
		if ((s_firstCachedValue <= value) && (value <= s_lastCachedValue))
		{
			initCache();
			return s_cache[value - s_firstCachedValue];
		}
		return null;
	}

	private static function initCache() : Void
	{
		if (s_cache == null)
		{
			s_cache = new Vector<BigInt_>(s_lastCachedValue + 1 - s_firstCachedValue);
			for (i in 0 ... s_cache.length)
			{
				s_cache[i] = newFromInt(i + s_firstCachedValue);
			}
		}
	}

	private var m_count : Int;
	private var m_data : Vector<Int>;

	private static inline var s_firstCachedValue : Int = -16;
	private static inline var s_lastCachedValue : Int = 16;
	private static var s_cache : Vector<BigInt_> = null;

	//-----------------------------------------------------------------------
	// Static helpers
	//-----------------------------------------------------------------------

	@:noCompletion
	private static inline function negate1(a : BigInt_) : BigInt_
	{
		var r = new MutableBigInt_();
		BigIntArithmetic.negate(r, a);
		return r;
	}

	@:noCompletion
	private static inline function equals2Int(a : BigInt_, b : Int) : Bool
	{
		return a.equalsInt(b);
	}

	@:noCompletion
	private static inline function equals2(a : BigInt_, b : BigInt_) : Bool
	{
		return a.equals(b);
	}

	@:noCompletion
	private static inline function addInt2(a : BigInt_, b : Int) : BigInt_
	{
		var r = new MutableBigInt_();
		BigIntArithmetic.addInt(r, a, b);
		return r;
	}

	@:noCompletion
	private static inline function add2(a : BigInt_, b : BigInt_) : BigInt_
	{
		var r = new MutableBigInt_();
		BigIntArithmetic.add(r, a, b);
		return r;
	}

	@:noCompletion
	private static inline function subInt2(a : BigInt_, b : Int) : BigInt_
	{
		var r = new MutableBigInt_();
		BigIntArithmetic.subtractInt(r, a, b);
		return r;
	}

	@:noCompletion
	private static inline function sub2(a : BigInt_, b : BigInt_) : BigInt_
	{
		var r = new MutableBigInt_();
		BigIntArithmetic.subtract(r, a, b);
		return r;
	}

	@:noCompletion
	private static inline function multiplyInt2(a : BigInt_, b : Int) : BigInt_
	{
		var r = new MutableBigInt_();
		BigIntArithmetic.multiplyInt(r, a, b);
		return r;
	}

	@:noCompletion
	private static inline function multiply2(a : BigInt_, b : BigInt_) : BigInt_
	{
		var r = new MutableBigInt_();
		BigIntArithmetic.multiply(r, a, b);
		return r;
	}

	@:noCompletion
	private static inline function divideInt2(a : BigInt_, b : Int) : BigInt_
	{
		var q = new MutableBigInt_();
		BigIntArithmetic.divideInt(a, b, q);
		return q;
	}

	@:noCompletion
	private static inline function divide2(a : BigInt_, b : BigInt_) : BigInt_
	{
		var q = new MutableBigInt_();
		BigIntArithmetic.divide(a, b, q, null);
		return q;
	}

	@:noCompletion
	private static inline function modulusInt2(a : BigInt_, b : Int) : Int
	{
		var q = new MutableBigInt_();
		return BigIntArithmetic.divideInt(a, b, q);
	}

	@:noCompletion
	private static inline function modulus2(a : BigInt_, b : BigInt_) : BigInt_
	{
		var q = new MutableBigInt_();
		var r = new MutableBigInt_();
		BigIntArithmetic.divide(a, b, q, r);
		return r;
	}

	@:noCompletion
	private static inline function arithmeticShiftLeft2(a : BigInt_, b : Int) : BigInt_
	{
		var r = new MutableBigInt_();
		BigIntArithmetic.arithmeticShiftLeft(r, a, b);
		return r;
	}

	@:noCompletion
	private static inline function arithmeticShiftRight2(a : BigInt_, b : Int) : BigInt_
	{
		var r = new MutableBigInt_();
		BigIntArithmetic.arithmeticShiftRight(r, a, b);
		return r;
	}

	@:noCompletion
	private static inline function sign1(a : BigInt_) : Int
	{
		return a.sign();
	}

	@:noCompletion
	private static inline function isZero1(a : BigInt_) : Bool
	{
		return a.isZero();
	}

	@:noCompletion
	private static inline function isNegative1(a : BigInt_) : Bool
	{
		return a.isNegative();
	}
	
	@:noCompletion
	private static inline function isPositive1(a : BigInt_) : Bool
	{
		return a.isPositive();
	}

	@:noCompletion
	private static inline function isOdd1(a : BigInt_) : Bool
	{
		return a.isOdd();
	}

	@:noCompletion
	private static inline function isEven1(a : BigInt_) : Bool
	{
		return a.isEven();
	}

	@:noCompletion
	private static inline function toString1(a : BigInt_) : String
	{
		return a.toString();
	}

	@:noCompletion
	private static inline function toHex1(a : BigInt_) : String
	{
		return a.toHex();
	}

	@:noCompletion
	private static inline function toBytes1(a : BigInt_) : Bytes
	{
		return a.toBytes();
	}

	@:noCompletion
	private static inline function toInts1(a : BigInt_, v : Vector<Int>) : Int
	{
		return a.toInts(v);
	}
}
