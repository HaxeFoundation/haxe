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

import haxe.math.bigint.BigIntException;
import haxe.math.bigint.BigIntError;
import haxe.math.bigint.BigIntHelper;
import haxe.ds.Vector;

/* Original code courtesy Chuck Batson (github.com/cbatson) */
class BigIntArithmetic {
	/**
		Compare a big integer with an Int.

		Returns -1 if `a < b`; otherwise
		returns 1 if `a > b`; otherwise
		returns 0 (`a == b`).
	**/
	public static function compareInt(a:BigInt_, b:Int):Int {
		if (a.m_count > 1) {
			return (a.sign() << 1) + 1;
		}
		var x:Int = a.m_data.get(0);
		var lt:Int = (x - b) ^ ((x ^ b) & ((x - b) ^ x)); // "Hacker's Delight" p. 23
		var gt:Int = (b - x) ^ ((x ^ b) & ((b - x) ^ b));
		return (lt >> 31) | (gt >>> 31);
	}

	/**
		Compare two big integers.

		Returns -1 if `a < b`; otherwise
		returns 1 if `a > b`; otherwise
		returns 0 (`a == b`).
	**/
	public static function compare(a:BigInt_, b:BigInt_):Int {
		if (a != b) {
			var c:Int = (a.sign() & 2) + (b.sign() & 1);
			switch (c) {
				case 0: // a and b are positive
					if (a.m_count > b.m_count) {
						return 1;
					}
					if (a.m_count < b.m_count) {
						return -1;
					}
				case 1: // a is positive, b is negative
					return 1;
				case 2: // a is negative, b is positive
					return -1;
				case 3: // a and b are negative
					if (a.m_count > b.m_count) {
						return -1;
					}
					if (a.m_count < b.m_count) {
						return 1;
					}
			}
			return MultiwordArithmetic.compareUnsigned(a.m_data, b.m_data, a.m_count);
		}
		return 0;
	}

	/**
		Perform the unary negation of big integer `operand` and put
		the result into big integer `result`.

		Ok for `result` and `operand` to be the same object.
	**/
	public static function negate(result:MutableBigInt_, operand:BigInt_):Void {
		var c:Int = 1;
		var x:Int32 = 0;
		var z:Int = 0;
		result.ensureCapacity(operand.m_count + 1, result == operand); // overflow may add a digit
		for (i in 0...operand.m_count) {
			x = ~operand.m_data.get(i);
			z = x + c;
			result.m_data.set(i, z);
			c = (x & ~z) >>> 31; // "Hacker's Delight" p. 38
		}
		result.m_count = operand.m_count;
		// detect overflow; intuitively, this can only occur for inputs of 2 ^ (32 * N - 1).
		if ((~x & z) < 0) {
			result.m_data.set(result.m_count++, 0);
		} else {
			// Handle compacting; intuitively, this can only occur for inputs of -[2 ^ (32 * N - 1)].
			// TODO: good way to detect this specific scenario?
			result.compact();
		}
	}

	/**
		Add big integer `operand2` to big integer `operand1` and put
		the result into big integer `result`.

		Ok for `result`, `operand1`, and `operand2` to be the same object.
	**/
	public static function add(result:MutableBigInt_, operand1:BigInt_, operand2:BigInt_):Void {
		var c:Int = 0;
		var x:Int32 = 0, y:Int32 = 0, z:Int = 0;
		if (operand1.m_count == operand2.m_count) {
			result.ensureCapacity(operand1.m_count + 1, (result == operand1) || (result == operand2));
			for (i in 0...operand1.m_count) {
				x = operand1.m_data.get(i);
				y = operand2.m_data.get(i);
				z = x + y + c;
				result.m_data.set(i, z);
				c = ((x & y) | ((x | y) & ~z)) >>> 31; // "Hacker's Delight" p. 38
			}
			result.m_count = operand1.m_count;
		} else {
			// longer operand is put into o1
			var o1 = (operand1.m_count > operand2.m_count) ? operand1 : operand2;
			var o2 = (operand1.m_count > operand2.m_count) ? operand2 : operand1;
			result.ensureCapacity(o1.m_count + 1, (result == operand1) || (result == operand2));
			var s:Int = o2.sign();
			for (i in 0...o2.m_count) {
				x = o1.m_data.get(i);
				y = o2.m_data.get(i);
				z = x + y + c;
				result.m_data.set(i, z);
				c = ((x & y) | ((x | y) & ~z)) >>> 31; // "Hacker's Delight" p. 38
			}
			y = s;
			for (i in o2.m_count...o1.m_count) {
				x = o1.m_data.get(i);
				z = x + y + c;
				result.m_data.set(i, z);
				c = ((x & y) | ((x | y) & ~z)) >>> 31; // "Hacker's Delight" p. 38
			}
			result.m_count = o1.m_count;
		}
		var o:Int = (z ^ x) & (z ^ y); // "Hacker's Delight" p. 29
		if (o < 0) // overflow flag is in sign bit
		{
			result.m_data.set(result.m_count++, ~(z >> 31));
		} else {
			result.compact(); // TODO: True that this will only ever eliminate at most one digit? Lighter way to detect?
		}
	}

	/**
		Add integer `operand2` to big integer `operand1` and put the
		result into big integer `result`.

		Ok for `result` and `operand1` to be the same object.
	**/
	public static function addInt(result:MutableBigInt_, operand1:BigInt_, operand2:Int):Void {
		var c:Int = 0;
		var x:Int32;
		var y:Int = operand2;
		var z:Int;

		result.ensureCapacity(operand1.m_count + 1, result == operand1);
		if (operand1.m_count > 1) {
			x = operand1.m_data.get(0);
			z = x + y;
			c = ((x & y) | ((x | y) & ~z)) >>> 31; // "Hacker's Delight" p. 38
			result.m_data.set(0, z);
			y >>= 31;
			for (i in 1...operand1.m_count - 1) {
				x = operand1.m_data.get(i);
				z = x + y + c;
				result.m_data.set(i, z);
				c = ((x & y) | ((x | y) & ~z)) >>> 31; // "Hacker's Delight" p. 38
			}
		}
		x = operand1.m_data.get(operand1.m_count - 1);
		z = x + y + c;
		result.m_data.set(operand1.m_count - 1, z);
		result.m_count = operand1.m_count;
		var o:Int = (z ^ x) & (z ^ y); // "Hacker's Delight" p. 29
		if (o < 0) // overflow flag is in sign bit
		{
			result.m_data.set(result.m_count++, x >> 31);
		} else if (result.m_count > 1) {
			if (z == (result.m_data.get(result.m_count - 2) >> 31)) {
				--result.m_count;
			}
		}
	}

	/**
		Subtract big integer `operand2` from big integer `operand1`
		and put the result into big integer `result`.

		Ok for `result`, `operand1`, and `operand2` to be the same object.
	**/
	public static function subtract(result:MutableBigInt_, operand1:BigInt_, operand2:BigInt_):Void {
		var c:Int32 = 0;
		var x:Int = 0, y:Int = 0, z:Int = 0;
		if (operand1.m_count == operand2.m_count) {
			result.ensureCapacity(operand1.m_count + 1, (result == operand1) || (result == operand2));
			for (i in 0...operand1.m_count) {
				x = operand1.m_data.get(i);
				y = operand2.m_data.get(i);
				z = x - y - c;
				result.m_data.set(i, z);
				c = ((~x & y) | (~(x ^ y) & z)) >>> 31; // "Hacker's Delight" p. 38
			}
			result.m_count = operand1.m_count;
		} else if (operand1.m_count > operand2.m_count) {
			// operand1 is longer
			result.ensureCapacity(operand1.m_count + 1, (result == operand1) || (result == operand2));
			var s:Int = operand2.sign();
			for (i in 0...operand2.m_count) {
				x = operand1.m_data.get(i);
				y = operand2.m_data.get(i);
				z = x - y - c;
				result.m_data.set(i, z);
				c = ((~x & y) | (~(x ^ y) & z)) >>> 31; // "Hacker's Delight" p. 38
			}
			y = s;
			for (i in operand2.m_count...operand1.m_count) {
				x = operand1.m_data.get(i);
				z = x - y - c;
				result.m_data.set(i, z);
				c = ((~x & y) | (~(x ^ y) & z)) >>> 31; // "Hacker's Delight" p. 38
			}
			result.m_count = operand1.m_count;
		} else {
			// operand2 is longer
			result.ensureCapacity(operand2.m_count + 1, (result == operand1) || (result == operand2));
			var s:Int = operand1.sign();
			for (i in 0...operand1.m_count) {
				x = operand1.m_data.get(i);
				y = operand2.m_data.get(i);
				z = x - y - c;
				result.m_data.set(i, z);
				c = ((~x & y) | (~(x ^ y) & z)) >>> 31; // "Hacker's Delight" p. 38
			}
			x = s;
			for (i in operand1.m_count...operand2.m_count) {
				y = operand2.m_data.get(i);
				z = x - y - c;
				result.m_data.set(i, z);
				c = ((~x & y) | (~(x ^ y) & z)) >>> 31; // "Hacker's Delight" p. 38
			}
			result.m_count = operand2.m_count;
		}
		var o:Int = (x ^ y) & (z ^ x); // "Hacker's Delight" p. 29
		if (o < 0) // overflow flag is in sign bit
		{
			result.m_data.set(result.m_count++, ~(z >> 31));
		} else {
			result.compact(); // TODO: True that this will only ever eliminate at most one digit? Lighter way to detect?
		}
	}

	/**
		Subtract integer `operand2` from big integer `operand1` and
		put the result into big integer `result`.

		Ok for `result` and `operand1` to be the same object.
	**/
	public static function subtractInt(result:MutableBigInt_, operand1:BigInt_, operand2:Int):Void {
		var c:Int = 0;
		var x:Int;
		var y:Int = operand2;
		var z:Int;

		result.ensureCapacity(operand1.m_count + 1, result == operand1);
		if (operand1.m_count > 1) {
			x = operand1.m_data.get(0);
			z = x - y;
			c = ((~x & y) | (~(x ^ y) & z)) >>> 31; // "Hacker's Delight" p. 38
			result.m_data.set(0, z);
			y >>= 31;
			for (i in 1...operand1.m_count - 1) {
				x = operand1.m_data.get(i);
				z = x - y - c;
				result.m_data.set(i, z);
				c = ((~x & y) | (~(x ^ y) & z)) >>> 31; // "Hacker's Delight" p. 38
			}
		}
		x = operand1.m_data.get(operand1.m_count - 1);
		z = x - y - c;
		result.m_data.set(operand1.m_count - 1, z);
		result.m_count = operand1.m_count;
		var o:Int = (x ^ y) & (z ^ x); // "Hacker's Delight" p. 29
		if (o < 0) // overflow flag is in sign bit
		{
			result.m_data.set(result.m_count++, x >> 31);
		} else if (result.m_count > 1) {
			if (z == (result.m_data.get(result.m_count - 2) >> 31)) {
				--result.m_count;
			}
		}
	}

	/**
		Multiply big integer `operand1` by integer `operand2` and put
		the result into `result`.

		`result` may not refer the same object as either `operand1`
		or `operand2`; however, `operand1` and `operand2` may be the
		same object.
	**/
	public static function multiplyInt(result:MutableBigInt_, operand1:BigInt_, operand2:Int):Void {
		// TODO: Optimize.
		multiply(result, operand1, BigInt_.fromInt(operand2));
	}

	/**
		Multiply big integer `operand1` by big integer `operand2` and
		put the result into `result`.

		`result` may not refer the same object as either `operand1`
		or `operand2`; however, `operand1` and `operand2` may be the
		same object.
	**/
	public static function multiply(result:MutableBigInt_, operand1:BigInt_, operand2:BigInt_):Void {
		// Implements Figure 8-1 (p. 172) from "Hacker's Delight", Second Edition; Henry S. Warren, Jr.; 2013.

		if ((operand1 == result) || (operand2 == result)) {
			throw new BigIntException(BigIntError.INVALID_ARGUMENT);
		}

		if (operand1.isZero() || operand2.isZero()) {
			result.setFromInt(0);
			return;
		}

		if ((operand1.bitLength() >= 2500) && (operand2.bitLength() >= 2500)) {
			multiplyKaratsuba(result, operand1, operand2);
			return;
		}

		var resultSize:Int = operand1.m_count + operand2.m_count;
		result.ensureCapacity(resultSize, false); // always overwrite result
		for (i in 0...resultSize) {
			result.m_data.set(i, 0);
		}
		result.m_count = resultSize;

		var b:Int, k:Int, t:Int;
		var u:Int, v:Int, w:Int;
		var m:Int = operand1.m_count << 1;
		var n:Int = operand2.m_count << 1;

		for (j in 0...n) {
			v = operand2.getShort(j);
			k = 0;
			for (i in 0...m) {
				u = operand1.getShort(i);
				w = result.getShort(i + j);
				t = u * v + w + k;
				result.setShort(i + j, t);
				k = t >>> 16;
			}
			result.setShort(j + m, k);
		}

		// Now result has the unsigned product.  Correct by
		// subtracting v * 2 ^ (16m) if u < 0, and
		// subtracting u * 2 ^ (16n) if v < 0.
		// TODO: Do these as 32-bit operations.
		if (operand1.isNegative()) {
			b = 0;
			for (j in 0...n) {
				w = result.getShort(j + m);
				v = operand2.getShort(j);
				t = w - v - b;
				result.setShort(j + m, t);
				b = t >>> 31;
			}
		}
		if (operand2.isNegative()) {
			b = 0;
			for (i in 0...m) {
				w = result.getShort(i + n);
				u = operand1.getShort(i);
				t = w - u - b;
				result.setShort(i + n, t);
				b = t >>> 31;
			}
		}

		result.compact();
	}

	/**
		Divide the big integer `dividend` by the integer `divisor`.
		The quotient of the division is put into `quotientOut`;
		the remainder is the return value.

		`quotientOut` may refer to `dividend`.

		`work`, if supplied, must not refer to any of the inputs.
	**/
	public static function divideInt(dividend:BigInt_, divisor:Int, quotientOut:MutableBigInt_, work:MutableBigInt_ = null):Int {
		// TODO: Consider optimizing this case.
		var remainder = new MutableBigInt_();
		var divisorBi = BigInt_.fromInt(divisor);
		divide(dividend, divisorBi, quotientOut, remainder, work);
		return remainder.m_data.get(0);
	}

	/**
		Divide the big integer `dividend` by the big integer `divisor`.
		The quotient of the division is put into `quotientOut`;
		the remainder is put into `remainderOut`.

		`remainderOut` may be `null` if the remainder value is not
		needed.

		`dividend` and `divisor` may refer to the same object.

		`quotientOut` and `remainderOut` must not refer to the same
		object; but either may refer to the inputs.

		`work`, if supplied, must not refer to any of the inputs.
	**/
	public static function divide(dividend:BigInt_, divisor:BigInt_, quotientOut:MutableBigInt_, remainderOut:MutableBigInt_,
			work:MutableBigInt_ = null):Void {
		var c:Int = (dividend.sign() & 2) + (divisor.sign() & 1);
		switch (c) {
			case 0: // dividend positive, divisor positive
				multiwordUnsignedDivide(dividend, divisor, quotientOut, remainderOut, work);
			case 1: // dividend positive, divisor negative
				negate(quotientOut, divisor);
				multiwordUnsignedDivide(dividend, quotientOut, quotientOut, remainderOut, work);
				negate(quotientOut, quotientOut);
			case 2: // dividend negative, divisor positive
				negate(quotientOut, dividend);
				multiwordUnsignedDivide(quotientOut, divisor, quotientOut, remainderOut, work);
				negate(quotientOut, quotientOut);
				if (remainderOut != null) {
					negate(remainderOut, remainderOut);
				}
			case 3: // dividend negative, divisor negative
				if (remainderOut == null) {
					// TODO: use work buffer rather than creating an object here
					remainderOut = new MutableBigInt_();
				}
				negate(quotientOut, dividend);
				negate(remainderOut, divisor);
				multiwordUnsignedDivide(quotientOut, remainderOut, quotientOut, remainderOut, work);
				negate(remainderOut, remainderOut);
		}
	}

	/*
		Unsigned division; inputs must not be negative.

		`remainderOut` may be `null` if the remainder value is not
		needed.

		`dividend` and `divisor` may refer to the same object.

		`quotientOut` and `remainderOut` must not refer to the same
		object; but either may refer to the inputs.

		`work`, if supplied, must not refer to any of the inputs.
	 */
	private static function multiwordUnsignedDivide(dividend:BigInt_, divisor:BigInt_, quotientOut:MutableBigInt_, remainderOut:MutableBigInt_,
			work:MutableBigInt_ = null):Void {
		if ((quotientOut == null) || (dividend == null) || (divisor == null)) {
			throw new BigIntException(BigIntError.INVALID_ARGUMENT);
		}
		if ((work == dividend) || (work == divisor) || (work == quotientOut)) {
			throw new BigIntException(BigIntError.INVALID_ARGUMENT);
		}

		var dividendInts:Int = dividend.getUnsignedDigitCount();
		var divisorInts:Int = divisor.getUnsignedDigitCount();

		var quotientLength:Int = MultiwordArithmetic.getDivisionQuotientLengthUnsigned(dividendInts, divisorInts);

		if (remainderOut != null) {
			if (work == remainderOut) {
				throw new BigIntException(BigIntError.INVALID_ARGUMENT);
			}
			remainderOut.ensureCapacity(divisor.m_count, (remainderOut == dividend) || (remainderOut == divisor));
		}
		quotientOut.ensureCapacity(quotientLength + 1, (quotientOut == dividend) || (quotientOut == divisor)); // +1 in case we need leading 0 digit

		if (work == null) {
			work = new MutableBigInt_();
		}
		work.ensureCapacity(dividendInts + divisorInts + 1, false);

		MultiwordArithmetic.divideUnsigned(dividend.m_data, dividendInts, divisor.m_data, divisorInts, quotientOut.m_data,
			(remainderOut != null) ? remainderOut.m_data : null, work.m_data);

		quotientOut.m_count = quotientLength;
		if (quotientOut.isNegative()) {
			quotientOut.m_data.set(quotientOut.m_count++, 0);
		} else {
			quotientOut.compact();
		}

		if (remainderOut != null) {
			remainderOut.m_count = divisorInts;
			if (remainderOut.isNegative()) {
				remainderOut.m_data.set(remainderOut.m_count++, 0);
			} else {
				remainderOut.compact();
			}
		}
	}

	/**
		Shift big integer `operand1` to the left by `operand2` bits
		and put the result into big integer `result`.

		Ok for `result` and `operand1` to be the same object.
	**/
	public static function arithmeticShiftLeft(result:MutableBigInt_, operand1:BigInt_, operand2:Int):Void {
		if (operand2 < 0) {
			throw new BigIntException(BigIntError.INVALID_ARGUMENT);
		}

		if ((operand2 == 0) || operand1.isZero()) {
			result.copyFrom(operand1);
			return;
		}

		result.ensureCapacity(operand1.m_count + ((operand2 + 31) >> 5), result == operand1);

		var whole:Int = operand2 >> 5; // whole digits portion
		var n:Int = operand2 & 0x1f; // sub digit poortion
		if (n > 0) {
			asl32(result.m_data, whole, operand1.m_data, operand1.m_count, n);
			result.m_count = operand1.m_count + whole + 1;
			result.compact();
		} else if (whole > 0) {
			for (i in 0...operand1.m_count) {
				result.m_data.set(operand1.m_count - i - 1 + whole, operand1.m_data.get(operand1.m_count - i - 1));
			}
			result.m_count = operand1.m_count + whole;
		}
		for (i in 0...whole) {
			result.m_data.set(i, 0);
		}
	}

	/**
		Shift big integer `operand1` to the right by `operand2` bits
		and put the result into big integer `result`.

		Ok for `result` and `operand1` to be the same object.
	**/
	public static function arithmeticShiftRight(result:MutableBigInt_, operand1:BigInt_, operand2:Int):Void {
		if (operand2 < 0) {
			throw new BigIntException(BigIntError.INVALID_ARGUMENT);
		}

		if ((operand2 == 0) || operand1.isZero()) {
			result.copyFrom(operand1);
			return;
		}

		result.ensureCapacity(operand1.m_count, result == operand1);

		var whole:Int = operand2 >> 5; // whole digits portion
		var n:Int = operand2 & 0x1f; // sub digit poortion
		if (whole >= operand1.m_count) {
			result.m_data.set(0, operand1.sign());
			result.m_count = 1;
		} else if (n > 0) {
			MultiwordArithmetic._asr32(result.m_data, operand1.m_data, operand1.m_count, whole, n);
			result.m_count = operand1.m_count - whole;
			result.compact();
		} else if (whole > 0) {
			for (i in 0...operand1.m_count - whole) {
				result.m_data.set(i, operand1.m_data.get(i + whole));
			}
			result.m_count = operand1.m_count - whole;
		}
	}

	/**
		Returns the value, 0 or 1, of the bit at 2^`index` place.
	**/
	public static inline function getBit(value:BigInt_, index:Int):Int {
		return MultiwordArithmetic.getBitSigned(value.m_data, value.m_count, index);
	}

	/**
		Returns the bitwise AND of `operand1` with `operand2`.
	**/
	public static inline function bitwiseAndInt(operand1:BigInt_, operand2:Int):Int {
		return operand1.m_data.get(0) & operand2;
	}

	public static inline function bitwiseAnd(operand1:BigInt_, operand2:BigInt_):BigInt_ {
		var result:MutableBigInt_ = new MutableBigInt_();
		if ((operand1.m_count > operand2.m_count)) {
			result.m_count = (operand2.sign() == 0) ? operand2.m_count : operand1.m_count;
		} else {
			result.m_count = (operand1.sign() == 0) ? operand1.m_count : operand2.m_count;
		}
		result.ensureCapacity(result.m_count, false);
		for (i in 0...result.m_count) {
			if (i > (operand1.m_count - 1)) {
				result.m_data.set(i, operand2.m_data.get(i));
			} else if (i > (operand2.m_count - 1)) {
				result.m_data.set(i, operand1.m_data.get(i));
			} else {
				result.m_data.set(i, (operand1.m_data.get(i) & operand2.m_data.get(i)));
			}
		}
		result.compact();
		return result;
	}

	/**
		Returns the bitwise OR of `operand1` with `operand2`.
	**/
	public static inline function bitwiseOr(operand1:BigInt_, operand2:BigInt_):BigInt_ {
		var result:MutableBigInt_ = new MutableBigInt_();
		result.m_count = (operand1.m_count > operand2.m_count) ? operand1.m_count : operand2.m_count;
		result.ensureCapacity(result.m_count, false);
		var operand1Positive:Bool = operand1.sign() == 0;
		var operand2Positive:Bool = operand2.sign() == 0;
		for (i in 0...result.m_count) {
			if (i > (operand1.m_count - 1)) {
				result.m_data.set(i, (operand1Positive ? operand2.m_data.get(i) : 0xffffffff));
			} else if (i > (operand2.m_count - 1)) {
				result.m_data.set(i, (operand2Positive ? operand1.m_data.get(i) : 0xffffffff));
			} else {
				result.m_data.set(i, (operand1.m_data.get(i) | operand2.m_data.get(i)));
			}
		}
		result.compact();
		return result;
	}

	public static inline function bitwiseXor(operand1:BigInt_, operand2:BigInt_):BigInt_ {
		var result:MutableBigInt_ = new MutableBigInt_();
		result.m_count = (operand1.m_count > operand2.m_count) ? operand1.m_count : operand2.m_count;
		result.ensureCapacity(result.m_count, false);
		var operand1Positive:Bool = operand1.sign() == 0;
		var operand2Positive:Bool = operand2.sign() == 0;
		for (i in 0...result.m_count) {
			if (i > (operand1.m_count - 1)) {
				result.m_data.set(i, (operand1Positive ? operand2.m_data.get(i) : (operand2.m_data.get(i) ^ 0xffffffff)));
			} else if (i > (operand2.m_count - 1)) {
				result.m_data.set(i, (operand2Positive ? operand1.m_data.get(i) : (operand1.m_data.get(i) ^ 0xffffffff)));
			} else {
				result.m_data.set(i, (operand1.m_data.get(i) ^ operand2.m_data.get(i)));
			}
		}
		result.compact();
		return result;
	}

	public static inline function bitwiseNot(operand:BigInt_):BigInt_ {
		var result:MutableBigInt_ = new MutableBigInt_();
		result.copyFrom(operand);
		for (i in 0...result.m_count) {
			result.m_data.set(i, ~operand.m_data.get(i));
		}
		result.compact();
		return result;
	}

	/**
		Returns `floor(log2(input))`.
	**/
	public static function floorLog2(input:BigInt_):Int {
		return (input.m_count << 5) - BigIntHelper.nlz(input.m_data.get(input.m_count - 1));
	}

	public static function multiplyKaratsuba(result:MutableBigInt_, x:BigInt_, y:BigInt_):Void {
		var n = (x.bitLength() > y.bitLength()) ? x.bitLength() : y.bitLength();
		if (n < 2500) {
			multiply(result, x, y);
			return;
		}
		n = (n + 1) >> 1;
		var b = new MutableBigInt_();
		arithmeticShiftRight(b, x, n);
		var a = new MutableBigInt_();
		arithmeticShiftLeft(a, b, n);
		subtract(a, x, a);
		var d = new MutableBigInt_();
		arithmeticShiftRight(d, y, n);
		var c = new MutableBigInt_();
		arithmeticShiftLeft(c, d, n);
		subtract(c, y, c);
		var ac = new MutableBigInt_();
		multiplyKaratsuba(ac, a, c);
		var bd = new MutableBigInt_();
		multiplyKaratsuba(bd, b, d);
		var abcd = new MutableBigInt_();
		add(a, a, b);
		add(c, c, d);
		multiplyKaratsuba(abcd, a, c);
		var op1 = new MutableBigInt_();
		arithmeticShiftLeft(op1, bd, 2 * n);
		var op2 = new MutableBigInt_();
		subtract(op2, abcd, ac);
		subtract(op2, op2, bd);
		arithmeticShiftLeft(op2, op2, n);
		add(op2, ac, op2);
		add(result, op1, op2);
	}

	//-----------------------------------------------------------------------
	// Private helpers
	//-----------------------------------------------------------------------
	// assumes 0 < shift < 32
	// ok if output == input
	private static inline function asl32(output:Vector<Int>, outputOffset:Int, input:Vector<Int>, inputSize:Int, shift:Int32):Void {
		var x:Int = input.get(inputSize - 1) >> 31; // sign extend
		var r:Int = 32 - shift;
		var y:Int;
		while (inputSize > 0) {
			y = input[inputSize - 1];
			x = (x << shift) | (y >>> r);
			output.set(inputSize + outputOffset, x);
			x = y;
			--inputSize;
		}
		output.set(outputOffset, x << shift);
	}

	// assumes 0 < shift < 32
	// ok if output == input
	private static inline function lsl32(output:Vector<Int>, outputOffset:Int, input:Vector<Int>, inputSize:Int, shift:Int32):Void {
		var x:Int = 0;
		var r:Int = 32 - shift;
		var y:Int;
		while (inputSize > 0) {
			y = input[inputSize - 1];
			x = (x << shift) | (y >>> r);
			output.set(inputSize + outputOffset, x);
			x = y;
			--inputSize;
		}
		output.set(outputOffset, x << shift);
	}

	// assumes 0 < shift < 32
	// ok if output == input
	private static inline function lsr32(output:Vector<Int>, input:Vector<Int>, inputSize:Int, inputOffset:Int, shift:Int32):Void {
		var r:Int = 32 - shift;
		var i:Int = 0;
		while (i < inputSize - 1) {
			output.set(i, (input.get(inputOffset + i) >>> shift) | (input.get(inputOffset + i + 1) << r));
			++i;
		}
		output.set(i, input.get(inputOffset + i) >>> shift);
	}

	private static inline function copy(output:Vector<Int>, outputOffset:Int, input:Vector<Int>, inputOffset:Int, length:Int):Void {
		for (i in 0...length) {
			output.set(outputOffset + i, input.get(inputOffset + i));
		}
	}
}
