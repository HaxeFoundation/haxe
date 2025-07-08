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
@:allow(haxe.math.bigint)
abstract BigInt(BigInt_) {
	//-----------------------------------------------------------------------
	// Public constants
	//-----------------------------------------------------------------------
	/** The BigInt value of 0 **/
	public static var ZERO(default, null):BigInt = new BigInt(BigInt_.fromInt(0));
	/** The BigInt value of 1 **/
	public static var ONE(default, null):BigInt = new BigInt(BigInt_.fromInt(1));
	/** The BigInt value of 2 **/
	public static var TWO(default, null):BigInt = new BigInt(BigInt_.fromInt(2));
	/** The BigInt value of -1 **/
	public static var MINUS_ONE(default, null):BigInt = new BigInt(BigInt_.fromInt(-1));
	
	//-----------------------------------------------------------------------
	// Private constants
	//-----------------------------------------------------------------------
	#if !cppia
	private static var SMALL_PRIMES_PRODUCT:BigInt = BigInt.fromString("1451887755777639901511587432083070202422614380984889313550570919659315177065956574359078912654149167643992684236991305777574330831666511589145701059710742276692757882915756220901998212975756543223550490431013061082131040808010565293748926901442915057819663730454818359472391642885328171302299245556663073719855");
	#end
	//-----------------------------------------------------------------------
	// Public interface
	//-----------------------------------------------------------------------

	/**
		Returns the sign of this `BigInt`.
		@return -1 if negative, 1 if positive, 0 if zero.
	**/
	public inline function sign():Int {
		return BigInt_.sign1(this);
	}

	/**
		Checks if this `BigInt` is equal to zero.
		@return `true` if the value is 0, otherwise `false`.
	**/
	public inline function isZero():Bool {
		return BigInt_.isZero1(this);
	}

	/**
		Checks if this `BigInt` is a negative number.
		@return `true` if the value is less than 0, otherwise `false`.
	**/
	public inline function isNegative():Bool {
		return BigInt_.isNegative1(this);
	}

	/**
		Checks if this `BigInt` is a positive number.
		@return `true` if the value is greater than 0, otherwise `false`.
	**/
	public inline function isPositive():Bool {
		return BigInt_.isPositive1(this);
	}

	/**
		Checks if this `BigInt` is an odd number.
		@return `true` if the value is odd, otherwise `false`.
	**/
	public inline function isOdd():Bool {
		return BigInt_.isOdd1(this);
	}

	/**
		Checks if this `BigInt` is an even number.
		@return `true` if the value is even, otherwise `false`.
	**/
	public inline function isEven():Bool {
		return BigInt_.isEven1(this);
	}

	/**
		Returns the minimum of this `BigInt` and another.
		@param other The `BigInt` to compare with.
		@return The smaller of the two `BigInt` values.
	**/
	public inline function min(other:BigInt):BigInt {
		return new BigInt(this.min(other));
	}

	/**
		Returns the maximum of this `BigInt` and another.
		@param other The `BigInt` to compare with.
		@return The larger of the two `BigInt` values.
	**/
	public inline function max(other:BigInt):BigInt {
		return new BigInt(this.max(other));
	}

	/**
		Returns the string representation of this `BigInt` in the specified base.
		@param radix The base for the conversion (e.g., 10 for decimal, 16 for hexadecimal).
		@return The string representation of the number.
	**/
	public inline function toString(radix:Int=10):String {
		return BigInt_.toString1(this,radix);
	}

	/**
		Returns the hexadecimal string representation of this `BigInt`.
		This is a shorthand for `toString(16)`.
		@return The hexadecimal string.
	**/
	public inline function toHex():String {
		return BigInt_.toHex1(this);
	}

	/**
		Converts this `BigInt` to a `Bytes` sequence (big-endian).
		@return A `Bytes` object representing the number's magnitude.
	**/
	public inline function toBytes():Bytes {
		return BigInt_.toBytes1(this);
	}

	/**
		Converts this `BigInt` to a `Vector` of `Int`s.
		@param output The vector to write the integer words into.
		@return The number of words written to the vector.
	**/
	public inline function toInts(output:Vector<Int>):Int {
		return BigInt_.toInts1(this, output);
	}

	/**
		Creates a new BigInt from an Int.
		@param value The Int value.
	**/
	public static inline function fromInt(value:Int):BigInt {
		return new BigInt(BigInt_.fromInt(value));
	}

	/**
		Creates a `BigInt` by parsing a string with a given radix.
		@param value The string representation of the number.
		@param radix The base of the number in the string (e.g., 10 or 16).
		@return A new `BigInt` instance.
	**/
	public static inline function fromString(value:String,radix:Int=10):BigInt {
		return new BigInt(BigInt_.fromString(value,radix));
	}

	/**
		Creates a `BigInt` from a hexadecimal string. Assumes unsigned representation.
		This is a shorthand for `fromHexUnsigned(value)`.
		@param value The hexadecimal string.
		@return A new `BigInt` instance.
	**/
	public static inline function fromHex(value:String):BigInt {
		return fromHexUnsigned(value);
	}

	/**
		Creates a `BigInt` from a signed hexadecimal string.
		@param value The hexadecimal string.
		@return A new `BigInt` instance.
	**/
	public static inline function fromHexSigned(value:String):BigInt {
		return new BigInt(BigInt_.fromHexSigned(value));
	}

	/**
		Creates a `BigInt` from an unsigned hexadecimal string.
		@param value The hexadecimal string.
		@return A new `BigInt` instance.
	**/
	public static inline function fromHexUnsigned(value:String):BigInt {
		return new BigInt(BigInt_.fromHexUnsigned(value));
	}

	/**
		Creates a `BigInt` from a `Vector` of unsigned `Int` words.
		@param value A vector containing the integer words of the number.
		@param length The number of words to use from the vector. If 0, uses the whole vector.
		@return A new `BigInt` instance.
	**/
	public static inline function fromUnsignedInts(value:Vector<Int>, length:Int = 0):BigInt {
		return new BigInt(BigInt_.fromUnsignedInts(value, length));
	}

	/**
		Creates a `BigInt` from a `Bytes` sequence.
		@param value The `Bytes` object to read from.
		@param offset The starting position in the bytes.
		@param length The number of bytes to read. If 0, reads to the end.
		@return A new `BigInt` instance.
	**/
	public static inline function fromBytes(value:Bytes, offset:Int = 0, length:Int = 0):BigInt {
		return new BigInt(BigInt_.fromBytes(value, offset, length));
	}

	/**
		Generates a pseudo-random `BigInt` with a specified number of bits.
		@param bits The bit-length of the random number. The result is in the range `[0, 2^bits - 1]`.
		@return A new `BigInt` with a random value.
	**/
	public static inline function random(bits:Int32):BigInt {
		return new BigInt(BigInt_.random(bits));
	}

	/**
		Generates a pseudo-random `BigInt` within a given range.
		@param min The inclusive minimum value.
		@param max The inclusive maximum value.
		@return A random `BigInt` such that `min <= result <= max`.
	**/
	public static function randomInRange(min:BigInt, max:BigInt):BigInt {
		return new BigInt(BigInt_.randomInRange(min, max));
	}

	/**
		Generates a probable prime number with a specified bit-length.
		@param bits The desired bit-length of the prime.
		@param tolerance The certainty level for the primality test.
		@return A probable prime `BigInt`.
	**/
	public static function randomPrime(bits:Int32, tolerance:UInt):BigInt {
		return new BigInt(BigInt_.randomPrime(bits, tolerance));
	}

	/**
		Performs division and returns both the quotient and the remainder.
		@param dividend The number to be divided.
		@param divisor The number to divide by.
		@return An object `{ quotient: BigInt, remainder: BigInt }`.
	**/
	public static function divMod(dividend:BigInt, divisor:BigInt):{quotient:BigInt, remainder:BigInt} {
		var result:{quotient:BigInt_, remainder:BigInt_} = BigInt_.divMod(dividend, divisor);
		return {quotient: (new BigInt(result.quotient)), remainder: (new BigInt(result.remainder))};
	}

	/**
		Gets the value of a single bit at the specified index.
		@param index The zero-based index of the bit to get.
		@return 1 if the bit is set, otherwise 0.
	**/
	public inline function getBit(index:Int):Int {
		return BigIntArithmetic.getBit(this, index);
	}

	/**
		Returns the absolute value of this `BigInt`.
		@return A new `BigInt` representing the absolute value.
	**/
	public function abs():BigInt {
		return new BigInt(this.abs());
	}

	/**
		Calculates the modular multiplicative inverse of this `BigInt`.
		@param modulus The modulus for the inverse operation.
		@return A new `BigInt` `x` such that `(this * x) % modulus == 1`.
	**/
	public function modInverse(modulus:BigInt_):BigInt {
		return new BigInt(this.modInverse(modulus));
	}

	/**
		Finds the greatest common divisor (GCD) of this `BigInt` and another.
		@param b The other `BigInt`.
		@return The GCD of this and `b`.
	**/
	public function gcd(b:BigInt):BigInt {
		return new BigInt(this.gcd(b));
	}

	/**
		Finds the least common multiple (LCM) of this `BigInt` and another.
		@param b The other `BigInt`.
		@return The LCM of this and `b`.
	**/
	public function lcm(b:BigInt):BigInt {
		return new BigInt(this.lcm(b));
	}

	/**
		Raises this `BigInt` to the power of an integer exponent.
		@param exponent The non-negative exponent.
		@return A new `BigInt` representing `this` raised to the power of `exponent`.
	**/
	public function pow(exponent:UInt):BigInt {
		return new BigInt(this.pow(exponent));
	}

	/**
		Calculates `(this ^ exponent) mod modulus`.
		@param exponent The exponent.
		@param modulus The modulus.
		@return The result of the modular exponentiation.
	**/
	public function modPow(exponent:BigInt, modulus:BigInt):BigInt {
		return new BigInt(this.modPow(exponent, modulus));
	}

	/**
		Calculates the square of this `BigInt`.
		@return A new `BigInt` representing `this * this`.
	**/
	public function square():BigInt {
		return new BigInt(this.square());
	}

	/**
		Performs a primality test on this `BigInt`.
		@param tolerance The certainty level. A higher value means a more rigorous (but slower) test.
		@return `true` if the number is probably prime, `false` if it is definitely composite.
	**/
	public function isProbablePrime(tolerance:UInt):Bool {
		return this.isProbablePrime(tolerance);
	}

	/**
		Finds the first probable prime number greater than this `BigInt`.
		@return The next probable prime.
	**/
	public function nextProbablePrime():BigInt {
		return new BigInt(this.nextProbablePrime());
	}

	/**
		Gets the index of the lowest-set (rightmost) bit.
		@return The index of the rightmost '1' bit, or -1 if the number is zero.
	**/
	public function getLowestSetBit():Int {
		return this.getLowestSetBit();
	}

	/**
		Returns the number of bits of this `BigInt`.
		@return The number of bits required to represent this number.
	**/
	public function bitLength():Int {
		return this.bitLength();
	}

	/**
		Counts the number of bits that are set to 1.
		@return The number of set bits.
	**/
	public function bitCount():Int {
		return this.bitCount();
	}

	/**
		Tests if the bit at a given index is set.
		@param n The index of the bit to test.
		@return `true` if the bit is 1, otherwise `false`.
	**/
	public function testBit(n:Int):Bool {
		return this.testBit(n);
	}

	/**
		Returns a new `BigInt` with the specified bit set (to 1).
		@param n The index of the bit to set.
		@return A new `BigInt` with the bit set.
	**/
	public function setBit(n:Int):BigInt {
		return new BigInt(this.setBit(n));
	}

	/**
		Returns a new `BigInt` with the specified bit cleared (to 0).
		@param n The index of the bit to clear.
		@return A new `BigInt` with the bit cleared.
	**/
	public function clearBit(n:Int):BigInt {
		return new BigInt(this.clearBit(n));
	}

	/**
		Returns a new `BigInt` with the specified bit flipped.
		@param n The index of the bit to flip.
		@return A new `BigInt` with the bit flipped.
	**/
	public function flipBit(n:Int):BigInt {
		return new BigInt(this.flipBit(n));
	}

	/**
		Calculates 2 raised to the power of the given exponent.
		@param exponent The exponent.
		@return A `BigInt` equal to `2^exponent`.
	**/
	public static function getPowerOfTwo(exponent:Int):BigInt {
		return new BigInt(BigInt_.getPowerOfTwo(exponent));
	}

	/**
		Generates a hash code for this `BigInt`.
		@return An integer hash code.
	**/
	public function hashCode():Int {
		return this.hashCode();
	}

	//-----------------------------------------------------------------------
	// Operators
	//-----------------------------------------------------------------------
	// The declaration order of the operations is significant in Haxe.
	// Recommended order is:
	//	* BigInt <binOp> Int
	//	* BigInt <binOp> BigInt
	//	* BigInt <binOp> MutableBigInt
	// Unary negation
	@:op(-A) @:noCompletion @:noDoc public static inline function negate_(a:BigInt):BigInt {
		return new BigInt(BigInt_.negate1(a));
	}

	// Binary equality
	@:op(A == B) @:noCompletion @:noDoc public static inline function eqInt_(a:BigInt, b:Int):Bool {
		return BigInt_.equals2Int(a, b);
	}

	@:op(A == B) @:noCompletion @:noDoc public static inline function eq_(a:BigInt, b:BigInt):Bool {
		return BigInt_.equals2(a, b);
	}

	@:op(A == B) @:noCompletion @:noDoc public static inline function eqMutable_<T:MutableBigInt_>(a:BigInt, b:T):Bool {
		return BigInt_.equals2(a, b);
	}

	// Binary inequality
	@:op(A != B) @:noCompletion @:noDoc public static inline function ineqInt_(a:BigInt, b:Int):Bool {
		return !BigInt_.equals2Int(a, b);
	}

	@:op(A != B) @:noCompletion @:noDoc public static inline function ineq_(a:BigInt, b:BigInt):Bool {
		return !BigInt_.equals2(a, b);
	}

	@:op(A != B) @:noCompletion @:noDoc public static inline function ineqMutable_<T:MutableBigInt_>(a:BigInt, b:T):Bool {
		return !BigInt_.equals2(a, b);
	}

	// Binary less than
	@:op(A < B) @:noCompletion @:noDoc public static inline function ltInt_(a:BigInt, b:Int):Bool {
		return BigIntArithmetic.compareInt(a, b) < 0;
	}

	@:op(A < B) @:noCompletion @:noDoc public static inline function lt_(a:BigInt, b:BigInt):Bool {
		return BigIntArithmetic.compare(a, b) < 0;
	}

	@:op(A < B) @:noCompletion @:noDoc public static inline function ltMutable_(a:BigInt, b:MutableBigInt):Bool {
		return BigIntArithmetic.compare(a, b) < 0;
	}

	// Binary less than or equal
	@:op(A <= B) @:noCompletion @:noDoc public static inline function lteInt_(a:BigInt, b:Int):Bool {
		return BigIntArithmetic.compareInt(a, b) <= 0;
	}

	@:op(A <= B) @:noCompletion @:noDoc public static inline function lte_(a:BigInt, b:BigInt):Bool {
		return BigIntArithmetic.compare(a, b) <= 0;
	}

	@:op(A <= B) @:noCompletion @:noDoc public static inline function lteMutable_(a:BigInt, b:MutableBigInt):Bool {
		return BigIntArithmetic.compare(a, b) <= 0;
	}

	// Binary greater than
	@:op(A > B) @:noCompletion @:noDoc public static inline function gtInt_(a:BigInt, b:Int):Bool {
		return BigIntArithmetic.compareInt(a, b) > 0;
	}

	@:op(A > B) @:noCompletion @:noDoc public static inline function gt_(a:BigInt, b:BigInt):Bool {
		return BigIntArithmetic.compare(a, b) > 0;
	}

	@:op(A > B) @:noCompletion @:noDoc public static inline function gtMutable_(a:BigInt, b:MutableBigInt):Bool {
		return BigIntArithmetic.compare(a, b) > 0;
	}

	// Binary greater than or equal
	@:op(A >= B) @:noCompletion @:noDoc public static inline function gteInt_(a:BigInt, b:Int):Bool {
		return BigIntArithmetic.compareInt(a, b) >= 0;
	}

	@:op(A >= B) @:noCompletion @:noDoc public static inline function gte_(a:BigInt, b:BigInt):Bool {
		return BigIntArithmetic.compare(a, b) >= 0;
	}

	@:op(A >= B) @:noCompletion @:noDoc public static inline function gteMutable_(a:BigInt, b:MutableBigInt):Bool {
		return BigIntArithmetic.compare(a, b) >= 0;
	}

	// Binary addition
	@:commutative @:op(A + B) @:noCompletion @:noDoc public static inline function addInt_(a:BigInt, b:Int):BigInt {
		return new BigInt(BigInt_.addInt2(a, b));
	}

	@:op(A + B) @:noCompletion @:noDoc public static inline function add_<T:BigInt_>(a:BigInt, b:T):BigInt {
		return new BigInt(BigInt_.add2(a, b));
	}

	@:op(A + B) @:noCompletion @:noDoc public static inline function addMutable_(a:BigInt, b:MutableBigInt):BigInt {
		return new BigInt(BigInt_.add2(a, b));
	}

	// Binary subtraction
	@:commutative @:op(A - B) @:noCompletion @:noDoc public static inline function subInt_(a:BigInt, b:Int):BigInt {
		return new BigInt(BigInt_.subInt2(a, b));
	}

	@:op(A - B) @:noCompletion @:noDoc public static inline function sub_(a:BigInt, b:BigInt):BigInt {
		return new BigInt(BigInt_.sub2(a, b));
	}

	@:op(A - B) @:noCompletion @:noDoc public static inline function subMutable_(a:BigInt, b:MutableBigInt):BigInt {
		return new BigInt(BigInt_.sub2(a, b));
	}

	// Binary multiplication
	@:commutative @:op(A * B) @:noCompletion @:noDoc public static inline function mulInt_(a:BigInt, b:Int):BigInt {
		return new BigInt(BigInt_.multiplyInt2(a, b));
	}

	@:op(A * B) @:noCompletion @:noDoc public static inline function mul_(a:BigInt, b:BigInt):BigInt {
		return new BigInt(BigInt_.multiply2(a, b));
	}

	@:op(A * B) @:noCompletion @:noDoc public static inline function mulMutable_(a:BigInt, b:MutableBigInt):BigInt {
		return new BigInt(BigInt_.multiply2(a, b));
	}

	// Binary division
	@:commutative @:op(A / B) @:noCompletion @:noDoc public static inline function divInt_(a:BigInt, b:Int):BigInt {
		return new BigInt(BigInt_.divideInt2(a, b));
	}

	@:op(A / B) @:noCompletion @:noDoc public static inline function div_(a:BigInt, b:BigInt):BigInt {
		return new BigInt(BigInt_.divide2(a, b));
	}

	@:op(A / B) @:noCompletion @:noDoc public static inline function divMutable_(a:BigInt, b:MutableBigInt):BigInt {
		return new BigInt(BigInt_.divide2(a, b));
	}

	// Binary modulus
	@:commutative @:op(A % B) @:noCompletion @:noDoc public static inline function modInt_(a:BigInt, b:Int):Int {
		return BigInt_.modulusInt2(a, b);
	}

	@:op(A % B) @:noCompletion @:noDoc public static inline function mod_(a:BigInt, b:BigInt):BigInt {
		return new BigInt(BigInt_.modulus2(a, b));
	}

	@:op(A % B) @:noCompletion @:noDoc public static inline function modMutable_(a:BigInt, b:MutableBigInt):BigInt {
		return new BigInt(BigInt_.modulus2(a, b));
	}

	// Binary OR
	@:op(A | B) @:noCompletion @:noDoc public static inline function orInt_(a:BigInt, b:Int):BigInt {
		return or_(a, b);
	}

	@:op(A | B) @:noCompletion @:noDoc public static inline function or_(a:BigInt, b:BigInt):BigInt {
		return new BigInt(BigIntArithmetic.bitwiseOr(a, b));
	}

	@:op(A ^ B) @:noCompletion @:noDoc public static inline function xorInt_(a:BigInt, b:Int):BigInt {
		return xor_(a, b);
	}

	@:op(A ^ B) @:noCompletion @:noDoc public static inline function xor_(a:BigInt, b:BigInt):BigInt {
		return new BigInt(BigIntArithmetic.bitwiseXor(a, b));
	}

	@:op(~A) @:noCompletion @:noDoc public static inline function not_(a:BigInt):BigInt {
		return new BigInt(BigIntArithmetic.bitwiseNot(a));
	}

	// Binary AND
	@:op(A & B) @:noCompletion @:noDoc public static inline function andInt_(a:BigInt, b:Int):Int {
		return BigIntArithmetic.bitwiseAndInt(a, b);
	}

	@:op(A & B) @:noCompletion @:noDoc public static inline function and_(a:BigInt, b:BigInt):BigInt {
		return new BigInt(BigIntArithmetic.bitwiseAnd(a, b));
	}

	// Binary shift left
	@:op(A << B) @:noCompletion @:noDoc public static inline function asl_(a:BigInt, b:Int):BigInt {
		return new BigInt(BigInt_.arithmeticShiftLeft2(a, b));
	}

	// Binary shift right
	@:op(A >> B) @:noCompletion @:noDoc public static inline function asr_(a:BigInt, b:Int):BigInt {
		return new BigInt(BigInt_.arithmeticShiftRight2(a, b));
	}

	//-----------------------------------------------------------------------
	// Automatic conversions
	//-----------------------------------------------------------------------

	@:from @:noCompletion @:noDoc public static inline function fromInt_(a:Int):BigInt {
		return new BigInt(BigInt_.fromInt(a));
	}

	@:from @:noCompletion @:noDoc public static inline function fromString_(a:String):BigInt {
		return new BigInt(BigInt_.fromString(a));
	}

	@:to @:noCompletion @:noDoc public inline function toBigInt_():BigInt_ {
		return this;
	}

	@:to @:noCompletion @:noDoc public inline function toMutableBigInt():MutableBigInt {
		return new MutableBigInt(MutableBigInt_.fromBigInt(this));
	}

	//-----------------------------------------------------------------------
	// Private implementation
	//-----------------------------------------------------------------------

	@:noCompletion @:noDoc private inline function new(a:BigInt_) {
		this = a;
	}
}
