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
@:allow(haxe.math.bigint)
abstract BigInt(BigInt_)
{
	//-----------------------------------------------------------------------
	// Public constants
	//-----------------------------------------------------------------------

	public static var ZERO(default, null) : BigInt = new BigInt(BigInt_.fromInt(0));
	public static var ONE(default, null) : BigInt = new BigInt(BigInt_.fromInt(1));
	public static var TWO(default, null) : BigInt = new BigInt(BigInt_.fromInt(2));
	public static var NEGATIVE_ONE(default, null) : BigInt = new BigInt(BigInt_.fromInt(-1));

	//-----------------------------------------------------------------------
	// Public interface
	//-----------------------------------------------------------------------

	public inline function sign() : Int
	{
		return BigInt_.sign1(this);
	}

	public inline function isZero() : Bool
	{
		return BigInt_.isZero1(this);
	}

	public inline function isNegative() : Bool
	{
		return BigInt_.isNegative1(this);
	}
	
	public inline function isPositive() : Bool
	{
		return BigInt_.isPositive1(this);
	}
	
	public inline function isOdd() : Bool
	{
		return BigInt_.isOdd1(this);
	}
	
	public inline function isEven() : Bool
	{
		return BigInt_.isEven1(this);
	}
	
	public inline function min(other : BigInt ) : BigInt {
		return new BigInt(this.min(other));
	}

	public inline function max(other : BigInt) : BigInt {
		return new BigInt(this.max(other));
	}

	public inline function toString() : String
	{
		return BigInt_.toString1(this);
	}

	public inline function toHex() : String
	{
		return BigInt_.toHex1(this);
	}

	public inline function toBytes() : Bytes
	{
		return BigInt_.toBytes1(this);
	}

	public inline function toInts(output : Vector<Int>) : Int
	{
		return BigInt_.toInts1(this, output);
	}

	public static inline function fromInt(value : Int) : BigInt
	{
		return new BigInt(BigInt_.fromInt(value));
	}

	public static inline function fromString(value : String) : BigInt
	{
		return new BigInt(BigInt_.fromString(value));
	}

	public static inline function fromHex(value : String) : BigInt
	{
		return fromHexSigned(value);
	}

	public static inline function fromHexSigned(value : String) : BigInt
	{
		return new BigInt(BigInt_.fromHexSigned(value));
	}

	public static inline function fromHexUnsigned(value : String) : BigInt
	{
		return new BigInt(BigInt_.fromHexUnsigned(value));
	}

	public static inline function fromUnsignedInts(value : Vector<Int>, length : Int = 0) : BigInt
	{
		return new BigInt(BigInt_.fromUnsignedInts(value, length));
	}
	
	public static inline function fromBytes(value : Bytes, offset : Int = 0, length : Int = 0) : BigInt
	{
		return new BigInt(BigInt_.fromBytes(value, offset, length));
	}
	
	public static inline function random(bits : Int32) : BigInt
	{
		return new BigInt(BigInt_.random(bits));
	}
	
	public static function randomInRange(min:BigInt, max:BigInt) : BigInt
	{
		return new BigInt(BigInt_.randomInRange(min,max));
	}

	public static function randomPrime(bits:Int32 , tolerance:UInt) : BigInt
	{
		return new BigInt(BigInt_.randomPrime(bits,tolerance));
	}

	public inline function getBit(index : Int) : Int
	{
		return BigIntArithmetic.getBit(this, index);
	}
	
	public function abs() : BigInt
	{
		return new BigInt(this.abs());
	}
	
	public function modInverse(modulus:BigInt_) : BigInt
	{
		return new BigInt(this.modInverse(modulus));
	}
	
	// Finds the greatest common denominator of this and b
	public function gcd(b:BigInt) : BigInt
	{
		return new BigInt(this.gcd(b));
	}
	
	public function lcm( b : BigInt) : BigInt
	{
		return new BigInt(this.lcm(b));
	}
	
	public function pow(exponent:UInt) : BigInt
	{
		return new BigInt(this.pow(exponent));
	}

	public function modPow(exponent:BigInt, modulus:BigInt) : BigInt
	{
		return new BigInt(this.modPow(exponent,modulus));
	}

	public function isProbablePrime(tolerance:UInt) : Bool
	{
		return this.isProbablePrime(tolerance);
	}
	
	public function nextProbablePrime() : BigInt
	{
		return new BigInt(this.nextProbablePrime());
	}

	public function getLowestSetBit():Int
	{
		return this.getLowestSetBit();
	}

	public function bitLength():Int {
		return this.bitLength();
	}
	
	public function bitCount():Int
	{
		return this.bitCount();
	}
	
	public function testBit(n:Int):Bool
	{
		return this.testBit(n);
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
	@:op(-A) @:noCompletion public static inline function negate_(a : BigInt) : BigInt
	{
		return new BigInt(BigInt_.negate1(a));
	}

	// Binary equality
	@:op(A == B) @:noCompletion public static inline function eqInt_(a : BigInt, b : Int) : Bool
	{
		return BigInt_.equals2Int(a, b);
	}
	@:op(A == B) @:noCompletion public static inline function eq_(a : BigInt, b : BigInt) : Bool
	{
		return BigInt_.equals2(a, b);
	}
	@:op(A == B) @:noCompletion public static inline function eqMutable_(a : BigInt, b : MutableBigInt) : Bool
	{
		return BigInt_.equals2(a, b);
	}

	// Binary inequality
	@:op(A != B) @:noCompletion public static inline function ineqInt_(a : BigInt, b : Int) : Bool
	{
		return !BigInt_.equals2Int(a, b);
	}
	@:op(A != B) @:noCompletion public static inline function ineq_(a : BigInt, b : BigInt) : Bool
	{
		return !BigInt_.equals2(a, b);
	}
	@:op(A != B) @:noCompletion public static inline function ineqMutable_(a : BigInt, b : MutableBigInt) : Bool
	{
		return !BigInt_.equals2(a, b);
	}

	// Binary less than
	@:op(A < B) @:noCompletion public static inline function ltInt_(a : BigInt, b : Int) : Bool
	{
		return BigIntArithmetic.compareInt(a, b) < 0;
	}
	@:op(A < B) @:noCompletion public static inline function lt_(a : BigInt, b : BigInt) : Bool
	{
		return BigIntArithmetic.compare(a, b) < 0;
	}
	@:op(A < B) @:noCompletion public static inline function ltMutable_(a : BigInt, b : MutableBigInt) : Bool
	{
		return BigIntArithmetic.compare(a, b) < 0;
	}

	// Binary less than or equal
	@:op(A <= B) @:noCompletion public static inline function lteInt_(a : BigInt, b : Int) : Bool
	{
		return BigIntArithmetic.compareInt(a, b) <= 0;
	}
	@:op(A <= B) @:noCompletion public static inline function lte_(a : BigInt, b : BigInt) : Bool
	{
		return BigIntArithmetic.compare(a, b) <= 0;
	}
	@:op(A <= B) @:noCompletion public static inline function lteMutable_(a : BigInt, b : MutableBigInt) : Bool
	{
		return BigIntArithmetic.compare(a, b) <= 0;
	}

	// Binary greater than
	@:op(A > B) @:noCompletion public static inline function gtInt_(a : BigInt, b : Int) : Bool
	{
		return BigIntArithmetic.compareInt(a, b) > 0;
	}
	@:op(A > B) @:noCompletion public static inline function gt_(a : BigInt, b : BigInt) : Bool
	{
		return BigIntArithmetic.compare(a, b) > 0;
	}
	@:op(A > B) @:noCompletion public static inline function gtMutable_(a : BigInt, b : MutableBigInt) : Bool
	{
		return BigIntArithmetic.compare(a, b) > 0;
	}

	// Binary greater than or equal
	@:op(A >= B) @:noCompletion public static inline function gteInt_(a : BigInt, b : Int) : Bool
	{
		return BigIntArithmetic.compareInt(a, b) >= 0;
	}
	@:op(A >= B) @:noCompletion public static inline function gte_(a : BigInt, b : BigInt) : Bool
	{
		return BigIntArithmetic.compare(a, b) >= 0;
	}
	@:op(A >= B) @:noCompletion public static inline function gteMutable_(a : BigInt, b : MutableBigInt) : Bool
	{
		return BigIntArithmetic.compare(a, b) >= 0;
	}

	// Binary addition
	@:op(A + B) @:noCompletion public static inline function addInt_(a : BigInt, b : Int) : BigInt
	{
		return new BigInt(BigInt_.addInt2(a, b));
	}
	@:op(A + B) @:noCompletion public static inline function add_(a : BigInt, b : BigInt) : BigInt
	{
		return new BigInt(BigInt_.add2(a, b));
	}
	@:op(A + B) @:noCompletion public static inline function addMutable_(a : BigInt, b : MutableBigInt) : BigInt
	{
		return new BigInt(BigInt_.add2(a, b));
	}

	// Binary subtraction
	@:op(A - B) @:noCompletion public static inline function subInt_(a : BigInt, b : Int) : BigInt
	{
		return new BigInt(BigInt_.subInt2(a, b));
	}
	@:op(A - B) @:noCompletion public static inline function sub_(a : BigInt, b : BigInt) : BigInt
	{
		return new BigInt(BigInt_.sub2(a, b));
	}
	@:op(A - B) @:noCompletion public static inline function subMutable_(a : BigInt, b : MutableBigInt) : BigInt
	{
		return new BigInt(BigInt_.sub2(a, b));
	}

	// Binary multiplication
	@:op(A * B) @:noCompletion public static inline function mulInt_(a : BigInt, b : Int) : BigInt
	{
		return new BigInt(BigInt_.multiplyInt2(a, b));
	}
	@:op(A * B) @:noCompletion public static inline function mul_(a : BigInt, b : BigInt) : BigInt
	{
		return new BigInt(BigInt_.multiply2(a, b));
	}
	@:op(A * B) @:noCompletion public static inline function mulMutable_(a : BigInt, b : MutableBigInt) : BigInt
	{
		return new BigInt(BigInt_.multiply2(a, b));
	}

	// Binary division
	@:op(A / B) @:noCompletion public static inline function divInt_(a : BigInt, b : Int) : BigInt
	{
		return new BigInt(BigInt_.divideInt2(a, b));
	}
	@:op(A / B) @:noCompletion public static inline function div_(a : BigInt, b : BigInt) : BigInt
	{
		return new BigInt(BigInt_.divide2(a, b));
	}
	@:op(A / B) @:noCompletion public static inline function divMutable_(a : BigInt, b : MutableBigInt) : BigInt
	{
		return new BigInt(BigInt_.divide2(a, b));
	}

	// Binary modulus
	@:op(A % B) @:noCompletion public static inline function modInt_(a : BigInt, b : Int) : Int
	{
		return BigInt_.modulusInt2(a, b);
	}
	@:op(A % B) @:noCompletion public static inline function mod_(a : BigInt, b : BigInt) : BigInt
	{
		return new BigInt(BigInt_.modulus2(a, b));
	}
	@:op(A % B) @:noCompletion public static inline function modMutable_(a : BigInt, b : MutableBigInt) : BigInt
	{
		return new BigInt(BigInt_.modulus2(a, b));
	}

	// Binary AND
	@:op(A & B) @:noCompletion public static inline function andInt_(a : BigInt, b : Int) : Int
	{
		return BigIntArithmetic.bitwiseAndInt(a, b);
	}

	// Binary shift left
	@:op(A << B) @:noCompletion public static inline function asl_(a : BigInt, b : Int) : BigInt
	{
		return new BigInt(BigInt_.arithmeticShiftLeft2(a, b));
	}

	// Binary shift right
	@:op(A >> B) @:noCompletion public static inline function asr_(a : BigInt, b : Int) : BigInt
	{
		return new BigInt(BigInt_.arithmeticShiftRight2(a, b));
	}

	//-----------------------------------------------------------------------
	// Automatic conversions
	//-----------------------------------------------------------------------

	@:from @:noCompletion public static inline function fromInt_(a : Int) : BigInt
	{
		return new BigInt(BigInt_.fromInt(a));
	}
	
	@:from @:noCompletion public static inline function fromString_(a : String) : BigInt
	{
		return new BigInt(BigInt_.fromString(a));
	}

	@:to @:noCompletion public inline function toBigInt_() : BigInt_
	{
		return this;
	}

	@:to @:noCompletion public inline function toMutableBigInt() : MutableBigInt
	{
		return new MutableBigInt(MutableBigInt_.fromBigInt(this));
	}

	//-----------------------------------------------------------------------
	// Private implementation
	//-----------------------------------------------------------------------

	@:noCompletion private inline function new(a : BigInt_)
	{
		this = a;
	}
	
	//-----------------------------------------------------------------------
	// Private constants
	//-----------------------------------------------------------------------
	
	private static var SMALL_PRIMES_PRODUCT: BigInt = BigInt.fromString("1451887755777639901511587432083070202422614380984889313550570919659315177065956574359078912654149167643992684236991305777574330831666511589145701059710742276692757882915756220901998212975756543223550490431013061082131040808010565293748926901442915057819663730454818359472391642885328171302299245556663073719855");

}
