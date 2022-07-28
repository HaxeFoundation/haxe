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
abstract MutableBigInt(MutableBigInt_)
{
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

	public inline function setFromInt(value : Int) : Void
	{
		var a : MutableBigInt_ = this;
		a.setFromInt(value);
	}

	public inline function setFromUnsignedInts(value : Vector<Int>, length : Int = 0) : Void
	{
		var a : MutableBigInt_ = this;
		a.setFromUnsignedInts(value, length);
	}

	public inline function setFromBigEndianBytesUnsigned(value : Bytes, offset : Int = 0, length : Int = 0) : Void
	{
		var a : MutableBigInt_ = this;
		a.setFromBigEndianBytesUnsigned(value, offset, length);
	}

	public inline function setFromLittleEndianBytesUnsigned(value : Bytes, offset : Int = 0, length : Int = 0) : Void
	{
		var a : MutableBigInt_ = this;
		a.setFromLittleEndianBytesUnsigned(value, offset, length);
	}

	public inline function clear() : Void
	{
		var a : MutableBigInt_ = this;
		a.clear();
	}

	public inline function copyFrom(other : BigInt) : Void
	{
		var a : MutableBigInt_ = this;
		a.copyFrom(other);
	}

	public inline function getBit(index : Int) : Int
	{
		return BigIntArithmetic.getBit(this, index);
	}

	public static function fromBigEndianBytesUnsigned(value : Bytes) : MutableBigInt
	{
		var r = new MutableBigInt_();
		r.setFromBigEndianBytesUnsigned(value);
		return new MutableBigInt(r);
	}

	public static function fromLittleEndianBytesUnsigned(value : Bytes) : MutableBigInt
	{
		var r = new MutableBigInt_();
		r.setFromLittleEndianBytesUnsigned(value);
		return new MutableBigInt(r);
	}
	
	public function abs() : MutableBigInt
	{
		return this.abs();
	}
	
	public function gcd(b:BigInt) : MutableBigInt
	{
		return this.gcd(b);
	}
	
	public function pow(exponent:UInt) : MutableBigInt
	{
		return this.pow(exponent);
	}
	
	public function modPow(exponent:BigInt, modulus:BigInt) : MutableBigInt
	{
		return this.modPow(exponent,modulus);
	}
	
	public function  isProbablePrime(tolerance:UInt):Bool
	{
		return this.isProbablePrime(tolerance);
	}
	
	public function getLowestSetBit():Int
	{
		return this.getLowestSetBit();
	}
	
	public function bitLength():Int {
		return this.bitLength();
	}

	//-----------------------------------------------------------------------
	// Operators
	//-----------------------------------------------------------------------

	// The declaration order of the operations is significant in Haxe.
	// Recommended order is:
	//	* MutableBigInt <binOp=> Int
	//	* MutableBigInt <binOp=> BigInt
	//	* MutableBigInt <binOp=> MutableBigInt
	//	* MutableBigInt <binOp> Int
	//	* MutableBigInt <binOp> BigInt
	//	* MutableBigInt <binOp> MutableBigInt

	// Unary negation
	@:op(-A) @:noCompletion public static inline function negate_(a : MutableBigInt) : BigInt
	{
		return new BigInt(BigInt_.negate1(a));
	}

	// Binary equality
	@:op(A == B) @:noCompletion public static inline function eqInt_(a : MutableBigInt, b : Int) : Bool
	{
		return BigInt_.equals2Int(a, b);
	}
	@:op(A == B) @:noCompletion public static inline function eq_(a : MutableBigInt, b : BigInt) : Bool
	{
		return BigInt_.equals2(a, b);
	}
	@:op(A == B) @:noCompletion public static inline function eqMutable_(a : MutableBigInt, b : MutableBigInt) : Bool
	{
		return BigInt_.equals2(a, b);
	}

	// Binary inequality
	@:op(A != B) @:noCompletion public static inline function ineqInt_(a : MutableBigInt, b : Int) : Bool
	{
		return !BigInt_.equals2Int(a, b);
	}
	@:op(A != B) @:noCompletion public static inline function ineq_(a : MutableBigInt, b : BigInt) : Bool
	{
		return !BigInt_.equals2(a, b);
	}
	@:op(A != B) @:noCompletion public static inline function ineqMutable_(a : MutableBigInt, b : MutableBigInt) : Bool
	{
		return !BigInt_.equals2(a, b);
	}

	// Binary less than
	@:op(A < B) @:noCompletion public static inline function ltInt_(a : MutableBigInt, b : Int) : Bool
	{
		return BigIntArithmetic.compareInt(a, b) < 0;
	}
	@:op(A < B) @:noCompletion public static inline function lt_(a : MutableBigInt, b : BigInt) : Bool
	{
		return BigIntArithmetic.compare(a, b) < 0;
	}
	@:op(A < B) @:noCompletion public static inline function ltMutable_(a : MutableBigInt, b : MutableBigInt) : Bool
	{
		return BigIntArithmetic.compare(a, b) < 0;
	}

	// Binary less than or equal
	@:op(A <= B) @:noCompletion public static inline function lteInt_(a : MutableBigInt, b : Int) : Bool
	{
		return BigIntArithmetic.compareInt(a, b) <= 0;
	}
	@:op(A <= B) @:noCompletion public static inline function lte_(a : MutableBigInt, b : BigInt) : Bool
	{
		return BigIntArithmetic.compare(a, b) <= 0;
	}
	@:op(A <= B) @:noCompletion public static inline function lteMutable_(a : MutableBigInt, b : MutableBigInt) : Bool
	{
		return BigIntArithmetic.compare(a, b) <= 0;
	}

	// Binary greater than
	@:op(A > B) @:noCompletion public static inline function gtInt_(a : MutableBigInt, b : Int) : Bool
	{
		return BigIntArithmetic.compareInt(a, b) > 0;
	}
	@:op(A > B) @:noCompletion public static inline function gt_(a : MutableBigInt, b : BigInt) : Bool
	{
		return BigIntArithmetic.compare(a, b) > 0;
	}
	@:op(A > B) @:noCompletion public static inline function gtMutable_(a : MutableBigInt, b : MutableBigInt) : Bool
	{
		return BigIntArithmetic.compare(a, b) > 0;
	}

	// Binary greater than or equal
	@:op(A >= B) @:noCompletion public static inline function gteInt_(a : MutableBigInt, b : Int) : Bool
	{
		return BigIntArithmetic.compareInt(a, b) >= 0;
	}
	@:op(A >= B) @:noCompletion public static inline function gte_(a : MutableBigInt, b : BigInt) : Bool
	{
		return BigIntArithmetic.compare(a, b) >= 0;
	}
	@:op(A >= B) @:noCompletion public static inline function gteMutable_(a : MutableBigInt, b : MutableBigInt) : Bool
	{
		return BigIntArithmetic.compare(a, b) >= 0;
	}

	// Binary addition
	@:op(A += B) @:noCompletion public static inline function addAssignInt_(a : MutableBigInt, b : Int) : MutableBigInt
	{
		BigIntArithmetic.addInt(a, a, b);
		return a;
	}
	@:op(A += B) @:noCompletion public static inline function addAssign_(a : MutableBigInt, b : BigInt) : MutableBigInt
	{
		BigIntArithmetic.add(a, a, b);
		return a;
	}
	@:op(A += B) @:noCompletion public static inline function addAssignMutable_(a : MutableBigInt, b : MutableBigInt) : MutableBigInt
	{
		BigIntArithmetic.add(a, a, b);
		return a;
	}
	@:op(A + B) @:noCompletion public static inline function addInt_(a : MutableBigInt, b : Int) : BigInt
	{
		return new BigInt(BigInt_.addInt2(a, b));
	}
	@:op(A + B) @:noCompletion public static inline function add_(a : MutableBigInt, b : BigInt) : BigInt
	{
		return new BigInt(BigInt_.add2(a, b));
	}
	@:op(A + B) @:noCompletion public static inline function addMutable_(a : MutableBigInt, b : MutableBigInt) : BigInt
	{
		return new BigInt(BigInt_.add2(a, b));
	}

	// Binary subtraction
	@:op(A -= B) @:noCompletion public static inline function subAssignInt_(a : MutableBigInt, b : Int) : MutableBigInt
	{
		BigIntArithmetic.subtractInt(a, a, b);
		return a;
	}
	@:op(A -= B) @:noCompletion public static inline function subAssign_(a : MutableBigInt, b : BigInt) : MutableBigInt
	{
		BigIntArithmetic.subtract(a, a, b);
		return a;
	}
	@:op(A -= B) @:noCompletion public static inline function subAssignMutable_(a : MutableBigInt, b : MutableBigInt) : MutableBigInt
	{
		BigIntArithmetic.subtract(a, a, b);
		return a;
	}
	@:op(A - B) @:noCompletion public static inline function subInt_(a : MutableBigInt, b : Int) : BigInt
	{
		return new BigInt(BigInt_.subInt2(a, b));
	}
	@:op(A - B) @:noCompletion public static inline function sub_(a : MutableBigInt, b : BigInt) : BigInt
	{
		return new BigInt(BigInt_.sub2(a, b));
	}
	@:op(A - B) @:noCompletion public static inline function subMutable_(a : MutableBigInt, b : MutableBigInt) : BigInt
	{
		return new BigInt(BigInt_.sub2(a, b));
	}

	// Binary multiplication
	@:op(A *= B) @:noCompletion public static inline function mulAssignInt_(a : MutableBigInt, b : Int) : MutableBigInt
	{
		MutableBigInt_.multiplyAssignInt2(a, b);
		return a;
	}
	@:op(A *= B) @:noCompletion public static inline function mulAssign_(a : MutableBigInt, b : BigInt) : MutableBigInt
	{
		MutableBigInt_.multiplyAssign2(a, b);
		return a;
	}
	@:op(A *= B) @:noCompletion public static inline function mulAssignMutable_(a : MutableBigInt, b : MutableBigInt) : MutableBigInt
	{
		MutableBigInt_.multiplyAssign2(a, b);
		return a;
	}
	@:op(A * B) @:noCompletion public static inline function mulInt_(a : MutableBigInt, b : Int) : BigInt
	{
		return new BigInt(BigInt_.multiplyInt2(a, b));
	}
	@:op(A * B) @:noCompletion public static inline function mul_(a : MutableBigInt, b : BigInt) : BigInt
	{
		return new BigInt(BigInt_.multiply2(a, b));
	}
	@:op(A * B) @:noCompletion public static inline function mulMutable_(a : MutableBigInt, b : MutableBigInt) : BigInt
	{
		return new BigInt(BigInt_.multiply2(a, b));
	}

	// Binary division
	@:op(A /= B) @:noCompletion public static inline function divAssignInt_(a : MutableBigInt, b : Int) : MutableBigInt
	{
		MutableBigInt_.divideAssignInt2(a, b);
		return a;
	}
	@:op(A /= B) @:noCompletion public static inline function divAssign_(a : MutableBigInt, b : BigInt) : MutableBigInt
	{
		MutableBigInt_.divideAssign2(a, b);
		return a;
	}
	@:op(A /= B) @:noCompletion public static inline function divAssignMutable_(a : MutableBigInt, b : MutableBigInt) : MutableBigInt
	{
		MutableBigInt_.divideAssign2(a, b);
		return a;
	}
	@:op(A / B) @:noCompletion public static inline function divInt_(a : MutableBigInt, b : Int) : BigInt
	{
		return new BigInt(BigInt_.divideInt2(a, b));
	}
	@:op(A / B) @:noCompletion public static inline function div_(a : MutableBigInt, b : BigInt) : BigInt
	{
		return new BigInt(BigInt_.divide2(a, b));
	}
	@:op(A / B) @:noCompletion public static inline function divMutable_(a : MutableBigInt, b : MutableBigInt) : BigInt
	{
		return new BigInt(BigInt_.divide2(a, b));
	}

	// Binary modulus
	@:op(A %= B) @:noCompletion public static inline function modAssignInt_(a : MutableBigInt, b : Int) : MutableBigInt
	{
		MutableBigInt_.modulusAssignInt2(a, b);
		return a;
	}
	@:op(A %= B) @:noCompletion public static inline function modAssign_(a : MutableBigInt, b : BigInt) : MutableBigInt
	{
		MutableBigInt_.modulusAssign2(a, b);
		return a;
	}
	@:op(A %= B) @:noCompletion public static inline function modAssignMutable_(a : MutableBigInt, b : MutableBigInt) : MutableBigInt
	{
		MutableBigInt_.modulusAssign2(a, b);
		return a;
	}
	@:op(A % B) @:noCompletion public static inline function modInt_(a : MutableBigInt, b : Int) : Int
	{
		return BigInt_.modulusInt2(a, b);
	}
	@:op(A % B) @:noCompletion public static inline function mod_(a : MutableBigInt, b : BigInt) : BigInt
	{
		return new BigInt(BigInt_.modulus2(a, b));
	}
	@:op(A % B) @:noCompletion public static inline function modMutable_(a : MutableBigInt, b : MutableBigInt) : BigInt
	{
		return new BigInt(BigInt_.modulus2(a, b));
	}

	// Binary AND
	@:op(A & B) @:noCompletion public static inline function andInt_(a : MutableBigInt, b : Int) : Int
	{
		return BigIntArithmetic.bitwiseAndInt(a, b);
	}

	// Binary shift left
	@:op(A <<= B) @:noCompletion public static inline function arithmeticShiftLeftAssign_(a : MutableBigInt, b : Int) : MutableBigInt
	{
		MutableBigInt_.arithmeticShiftLeftAssign2(a, b);
		return a;
	}
	@:op(A << B) @:noCompletion public static inline function asl_(a : MutableBigInt, b : Int) : BigInt
	{
		return new BigInt(BigInt_.arithmeticShiftLeft2(a, b));
	}

	// Binary shift right
	@:op(A >>= B) @:noCompletion public static inline function arithmeticShiftRightAssign_(a : MutableBigInt, b : Int) : MutableBigInt
	{
		MutableBigInt_.arithmeticShiftRightAssign2(a, b);
		return a;
	}
	@:op(A >> B) @:noCompletion public static inline function asr_(a : MutableBigInt, b : Int) : BigInt
	{
		return new BigInt(BigInt_.arithmeticShiftRight2(a, b));
	}

	//-----------------------------------------------------------------------
	// Automatic conversions
	//-----------------------------------------------------------------------

	@:from @:noCompletion public static inline function fromInt_(a : Int) : MutableBigInt
	{
		return new MutableBigInt(MutableBigInt_.fromInt(a));
	}

	@:from @:noCompletion public static inline function fromBigInt_(a : BigInt_) : MutableBigInt
	{
		return new MutableBigInt(MutableBigInt_.fromBigInt(a));
	}

	@:from @:noCompletion public static inline function fromMutableBigInt_(a : MutableBigInt_) : MutableBigInt
	{
		return new MutableBigInt(MutableBigInt_.fromBigInt(a));
	}

	@:to @:noCompletion public inline function toMutableBigInt_() : MutableBigInt_
	{
		return this;
	}

	@:to @:noCompletion public inline function toBigInt() : BigInt
	{
		return new BigInt(this);
	}

	//-----------------------------------------------------------------------
	// Private implementation
	//-----------------------------------------------------------------------

	private inline function new(a : MutableBigInt_)
	{
		this = a;
	}
}
