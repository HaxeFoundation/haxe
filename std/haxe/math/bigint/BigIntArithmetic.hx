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
/**
	A collection of static helper functions for performing arithmetic
	on `BigInt_` objects.
**/
class BigIntArithmetic {
	/**
		Compare a big integer with an Int.

		Returns -1 if `a < b`; otherwise
		returns 1 if `a > b`; otherwise
		returns 0 (`a == b`).
	**/
	public static function compareInt(a:BigInt_, b:Int):Int {
		if (a.m_count > 1) {
			return a.sign();
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
			var c:Int = (a.sign() & 7) + (b.sign() & 3);
			switch (c) {
				case 1, 2: // a and b are positive
					if (a.m_count > b.m_count) {
						return 1;
					}
					if (a.m_count < b.m_count) {
						return -1;
					}
				case 3, 4: // a is positive, b is negative
					return 1;
				case 7, 8: // a is negative, b is positive
					return -1;
				case 10: // a and b are negative
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
			var s:Int = (o2.sign() == -1) ? -1 : 0;
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
			var s:Int = (operand2.sign() == -1) ? -1 : 0;
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
			var s:Int = (operand1.sign() == -1) ? -1 : 0;
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
	private static function multiplyTraditional(result:MutableBigInt_, operand1:BigInt_, operand2:BigInt_):Void {
		// Implements Figure 8-1 (p. 172) from "Hacker's Delight", Second Edition; Henry S. Warren, Jr.; 2013.

		if ((operand1 == result) || (operand2 == result)) {
			throw new BigIntException(BigIntError.INVALID_ARGUMENT);
		}

		if (operand1.isZero() || operand2.isZero()) {
			result.setFromInt(0);
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
		var dividendSign = dividend.sign();
		var divisorSign = divisor.sign();

		// Create a combined case value: 0 = both positive, 1 = dividend positive/divisor negative,
		// 2 = dividend negative/divisor positive, 3 = both negative
		var c:Int = 0;
		if (dividendSign == -1)
			c += 2;
		if (divisorSign == -1)
			c += 1;
			
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
			result.m_data.set(0, (operand1.sign() == -1) ? -1 : 0);
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

	/**
		Returns the bitwise AND of two big integers.
		@return A new `BigInt_` holding the result.
	**/
	public static inline function bitwiseAnd(operand1:BigInt_, operand2:BigInt_):BigInt_ {
		var result:MutableBigInt_ = new MutableBigInt_();
		if ((operand1.m_count > operand2.m_count)) {
			result.m_count = (operand2.sign() == 1) ? operand2.m_count : operand1.m_count;
		} else {
			result.m_count = (operand1.sign() == 1) ? operand1.m_count : operand2.m_count;
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
		var operand1Positive:Bool = operand1.sign() == 1;
		var operand2Positive:Bool = operand2.sign() == 1;
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

	/**
		Returns the bitwise XOR of two big integers.
		@return A new `BigInt_` holding the result.
	**/
	public static inline function bitwiseXor(operand1:BigInt_, operand2:BigInt_):BigInt_ {
		var result:MutableBigInt_ = new MutableBigInt_();
		result.m_count = (operand1.m_count > operand2.m_count) ? operand1.m_count : operand2.m_count;
		result.ensureCapacity(result.m_count, false);
		var operand1Positive:Bool = operand1.sign() == 1;
		var operand2Positive:Bool = operand2.sign() == 1;
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

	/**
		Returns the bitwise NOT (inversion) of a big integer.
		@return A new `BigInt_` holding the result.
	**/
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
		@param input The `BigInt_` operand.
		@return The integer base-2 logarithm.
	**/
	public static function floorLog2(input:BigInt_):Int {
		return (input.m_count << 5) - BigIntHelper.nlz(input.m_data.get(input.m_count - 1));
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
	
	private static function nextPowerOfTwo(value:Int):Int {
		if (value <= 0)
			return 1;
		value--;
		value |= value >> 1;
		value |= value >> 2;
		value |= value >> 4;
		value |= value >> 8;
		value |= value >> 16;
		return value + 1;
	}

	private static function splitAtPowerOfTwo(value:BigInt_, splitBits:Int, high:MutableBigInt_, low:MutableBigInt_):Void {
		if (splitBits <= 0) {
			high.setFromInt(0);
			low.copyFrom(value);
			return;
		}

		var wordBoundary:Int = splitBits >> 5;
		var bitOffset:Int = splitBits & 0x1f; // modulo 32

		if (wordBoundary >= value.m_count) {
			high.setFromInt(0);
			low.copyFrom(value);
			return;
		}

		low.ensureCapacity(wordBoundary + 1, false);

		// Copy the lower words
		for (i in 0...wordBoundary) {
			low.m_data.set(i, value.m_data.get(i));
		}

		if (bitOffset > 0 && wordBoundary < value.m_count) {
			var mask:Int = (1 << bitOffset) - 1;
			low.m_data.set(wordBoundary, value.m_data.get(wordBoundary) & mask);
			low.m_count = wordBoundary + 1;
		} else {
			low.m_count = wordBoundary;
		}
		
		// Ensure low part is always treated as positive
		// by adding a leading zero if the most significant bit is set
		if (low.m_count > 0 && low.m_data.get(low.m_count - 1) < 0) {
			low.ensureCapacity(low.m_count + 1, false);
			low.m_data.set(low.m_count, 0);
			low.m_count++;
		}
		
		low.compact();

		BigIntArithmetic.arithmeticShiftRight(high, value, splitBits);
	}
	
	public static function multiply(result:MutableBigInt_, operand1:BigInt_, operand2:BigInt_):Void {
		if ((operand1 == result) || (operand2 == result)) {
			throw new BigIntException(BigIntError.INVALID_ARGUMENT);
		}

		if (operand1.isZero() || operand2.isZero()) {
			result.setFromInt(0);
			return;
		}

		var bitLength1 = operand1.bitLength();
		var bitLength2 = operand2.bitLength();

		if (bitLength1 < 512 || bitLength2 < 512) {
			BigIntArithmetic.multiplyTraditional(result, operand1, operand2);
			return;
		}

		var maxBits = (bitLength1 > bitLength2) ? bitLength1 : bitLength2;
		var splitBits = nextPowerOfTwo(maxBits >> 1);

		if (splitBits < 256)
			splitBits = 256;

		multiplyKaratsuba(result, operand1, operand2, splitBits);
	}
	
	private static function multiplyKaratsuba(result:MutableBigInt_, x:BigInt_, y:BigInt_, splitBits:Int):Void {
		if (x.bitLength() < 512 || y.bitLength() < 512) {
			BigIntArithmetic.multiplyTraditional(result, x, y);
			return;
		}

		var x1 = new MutableBigInt_();
		var x0 = new MutableBigInt_();
		splitAtPowerOfTwo(x, splitBits, x1, x0);

		var y1 = new MutableBigInt_();
		var y0 = new MutableBigInt_();
		splitAtPowerOfTwo(y, splitBits, y1, y0);

		var z2 = new MutableBigInt_(); // x1 * y1
		var z0 = new MutableBigInt_(); // x0 * y0
		var z1 = new MutableBigInt_(); // (x1 + x0) * (y1 + y0) - z2 - z0

		//  z2 = x1 * y1
		multiplyKaratsuba(z2, x1, y1, splitBits >> 1);

		//  z0 = x0 * y0
		multiplyKaratsuba(z0, x0, y0, splitBits >> 1);

		//  z1 = (x1 + x0) * (y1 + y0) - z2 - z0
		var sum_x = new MutableBigInt_();
		var sum_y = new MutableBigInt_();
		BigIntArithmetic.add(sum_x, x1, x0);
		BigIntArithmetic.add(sum_y, y1, y0);

		multiplyKaratsuba(z1, sum_x, sum_y, splitBits >> 1);
		BigIntArithmetic.subtract(z1, z1, z2);
		BigIntArithmetic.subtract(z1, z1, z0);

		// result = z2 * 2^(2*splitBits) + z1 * 2^splitBits + z0
		var temp1 = new MutableBigInt_();
		var temp2 = new MutableBigInt_();

		// z2 * 2^(2*splitBits)
		BigIntArithmetic.arithmeticShiftLeft(temp1, z2, 2 * splitBits);

		// z1 * 2^splitBits
		BigIntArithmetic.arithmeticShiftLeft(temp2, z1, splitBits);

		BigIntArithmetic.add(result, temp1, temp2);
		BigIntArithmetic.add(result, result, z0);
	}
	
	/**
		Squaring operation using power-of-two splitting.
		@param result The output BigInt for the square
		@param operand The operand to square
	**/
	public static function square(result:MutableBigInt_, operand:BigInt_):Void {
		if (operand == result) {
			throw new BigIntException(BigIntError.INVALID_ARGUMENT);
		}

		if (operand.isZero()) {
			result.setFromInt(0);
			return;
		}

		var bitLength = operand.bitLength();

		if (bitLength < 512) {
			BigIntArithmetic.multiplyTraditional(result, operand, operand);
			return;
		}

		var splitBits = nextPowerOfTwo(bitLength >> 1);
		if (splitBits < 256)
			splitBits = 256;

		var high = new MutableBigInt_();
		var low = new MutableBigInt_();
		splitAtPowerOfTwo(operand, splitBits, high, low);

		// Calculate (high + low)^2 = high^2 + 2*high*low + low^2
		var highSquared = new MutableBigInt_();
		var lowSquared = new MutableBigInt_();
		var crossProduct = new MutableBigInt_();

		square(highSquared, high);
		square(lowSquared, low);

		// Calculate 2 * high * low
		multiplyKaratsuba(crossProduct, high, low, splitBits >> 1);
		BigIntArithmetic.arithmeticShiftLeft(crossProduct, crossProduct, 1);

		// Combine results: result = high^2 * 2^(2*splitBits) + 2*high*low * 2^splitBits + low^2
		var temp1 = new MutableBigInt_();
		var temp2 = new MutableBigInt_();

		BigIntArithmetic.arithmeticShiftLeft(temp1, highSquared, 2 * splitBits);
		BigIntArithmetic.arithmeticShiftLeft(temp2, crossProduct, splitBits);

		BigIntArithmetic.add(result, temp1, temp2);
		BigIntArithmetic.add(result, result, lowSquared);
	}
}