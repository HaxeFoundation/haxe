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

@:allow(haxe.math.bigint)
class BigInt_
{
	//-----------------------------------------------------------------------
	// Public interface
	//-----------------------------------------------------------------------

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

	/**
		Retrieve the sign value of this big integer; 0 if positive, -1 if negative.
	**/
	public inline function sign() : Int
	{
		return m_data.get(m_count - 1) >> 31;
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
		Creates a big integer with the value represented by the integer vector `value`.
	**/
	public static function fromUnsignedInts(value : Vector<Int>, length : Int = 0) : BigInt_
	{
		var bi = new MutableBigInt_();
		bi.setFromUnsignedInts(value, length);
		return bi;
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

	private static function newFromInt(value : Int) : BigInt_
	{
		var bi = new MutableBigInt_();
		bi.setFromInt(value);
		return bi;
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
