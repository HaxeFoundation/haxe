/*
 * Copyright (C)2005-2017 Haxe Foundation
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

#if ((flash || flash9doc || cs || hl) && !doc_gen)
/**
	The unsigned `Int` type is only defined for Flash and C#. It's currently
	handled the same as a normal Int.

	@see https://haxe.org/manual/types-basic-types.html
**/
@:coreType
@:notNull
@:runtimeValue
@:analyzer(no_const_propagation)
abstract UInt to Int from Int
{
	@:commutative @:op(A+B) private static function addI(lhs:UInt, rhs:Int):UInt;
	@:commutative @:op(A+B) private static function addF(lhs:UInt, rhs:Float):Float;
	@:op(A+B) private static function add(lhs:UInt, rhs:UInt):UInt;

	@:commutative @:op(A*B) private static function mulI(lhs:UInt, rhs:Int):UInt;
	@:commutative @:op(A*B) private static function mulF(lhs:UInt, rhs:Float):Float;
	@:op(A*B) private static function mul(lhs:UInt, rhs:UInt):UInt;

	@:op(A%B) private static function modI(lhs:UInt, rhs:Int):UInt;
	@:op(A%B) private static function modF(lhs:UInt, rhs:Float):Float;
	@:op(A%B) private static function mod(lhs:UInt, rhs:UInt):UInt;

	@:op(A-B) private static function subI(lhs:UInt, rhs:Int):UInt;
	@:op(A-B) private static function subF(lhs:UInt, rhs:Float):Float;
	@:op(A-B) private static function sub(lhs:UInt, rhs:UInt):UInt;

	@:op(A/B) private static function divI(lhs:UInt, rhs:Int):Float;
	@:op(A/B) private static function divF(lhs:UInt, rhs:Float):Float;
	@:op(A/B) private static function div(lhs:UInt, rhs:UInt):Float;

	@:commutative @:op(A|B) private static function orI(lhs:UInt, rhs:Int):UInt;
	@:op(A|B) private static function or(lhs:UInt, rhs:UInt):UInt;

	@:commutative @:op(A^B) private static function xorI(lhs:UInt, rhs:Int):UInt;
	@:op(A^B) private static function xor(lhs:UInt, rhs:UInt):UInt;

	@:commutative @:op(A&B) private static function andI(lhs:UInt, rhs:Int):UInt;
	@:op(A&B) private static function and(lhs:UInt, rhs:UInt):UInt;

	@:op(A<<B) private static function shl(lhs:UInt, rhs:Int):UInt;
	@:op(A>>B) private static inline function shr(lhs:UInt, rhs:Int):UInt return lhs >>> rhs;
	@:op(A>>>B) private static function ushr(lhs:UInt, rhs:Int):UInt;

	@:op(A>B) private static function gt(lhs:UInt, rhs:UInt):Bool;
	@:op(A>=B) private static function gte(lhs:UInt, rhs:UInt):Bool;
	@:op(A<B) private static function lt(lhs:UInt, rhs:UInt):Bool;
	@:op(A<=B) private static function lte(lhs:UInt, rhs:UInt):Bool;

	@:op(A>B) private static function gtf(lhs:UInt, rhs:Float):Bool;
	@:op(A>B) private static function gtf2(lhs:Float, rhs:UInt):Bool;
	@:op(A>=B) private static function gtef(lhs:UInt, rhs:Float):Bool;
	@:op(A>=B) private static function gtef2(lhs:Float, rhs:UInt):Bool;
	@:op(A<B) private static function ltf(lhs:UInt, rhs:Float):Bool;
	@:op(A<B) private static function ltf2(lhs:Float, rhs:UInt):Bool;
	@:op(A<=B) private static function ltef(lhs:UInt, rhs:Float):Bool;
	@:op(A<=B) private static function ltef2(lhs:Float, rhs:UInt):Bool;

	@:op(~A) private static function bneg(t:UInt):UInt;

	@:commutative @:op(A == B) private static function equalsInt<T:Int>(a:UInt, b:T):Bool;
	@:commutative @:op(A != B) private static function notEqualsInt<T:Int>(a:UInt, b:T):Bool;
	@:commutative @:op(A == B) private static function equalsFloat<T:Float>(a:UInt, b:T):Bool;
	@:commutative @:op(A != B) private static function notEqualsFloat<T:Float>(a:UInt, b:T):Bool;

	@:op(++A) private function prefixIncrement():UInt;
	@:op(A++) private function postfixIncrement():UInt;
	@:op(--A) private function prefixDecrement():UInt;
	@:op(A--) private function postfixDecrement():UInt;
}
#else
/**
	The unsigned `Int` type is only defined for Flash and C#.
	Simulate it for other platforms.

	@see https://haxe.org/manual/types-basic-types.html
**/
abstract UInt(Int) from Int to Int {

	@:op(A + B) private static inline function add(a:UInt, b:UInt):UInt {
		return a.toInt() + b.toInt();
	}

	@:op(A / B) private static inline function div(a:UInt, b:UInt):Float {
		return a.toFloat() / b.toFloat();
	}

	@:op(A * B) private static inline function mul(a:UInt, b:UInt):UInt {
		return a.toInt() * b.toInt();
	}

	@:op(A - B) private static inline function sub(a:UInt, b:UInt):UInt {
		return a.toInt() - b.toInt();
	}

	@:op(A > B) private static #if !js inline #end function gt(a:UInt, b:UInt):Bool {
		var aNeg = a.toInt() < 0;
		var bNeg = b.toInt() < 0;
		return
			if( aNeg != bNeg ) aNeg;
			else a.toInt() > b.toInt();
	}

	@:op(A >= B) private static #if !js inline #end function gte(a:UInt, b:UInt):Bool {
		var aNeg = a.toInt() < 0;
		var bNeg = b.toInt() < 0;
		return
			if( aNeg != bNeg ) aNeg;
			else a.toInt() >= b.toInt();
	}

	@:op(A < B) private static inline function lt(a:UInt, b:UInt):Bool {
		return gt(b, a);
	}

	@:op(A <= B) private static inline function lte(a:UInt, b:UInt):Bool {
		return gte(b, a);
	}

	@:op(A & B) private static inline function and(a:UInt, b:UInt):UInt {
		return a.toInt() & b.toInt();
	}

	@:op(A | B) private static inline function or(a:UInt, b:UInt):UInt {
		return a.toInt() | b.toInt();
	}

	@:op(A ^ B) private static inline function xor(a:UInt, b:UInt):UInt {
		return a.toInt() ^ b.toInt();
	}

	@:op(A << B) private static inline function shl(a:UInt, b:Int):UInt {
		return a.toInt() << b;
	}

	@:op(A >> B) private static inline function shr(a:UInt, b:Int):UInt {
		return a.toInt() >>> b;
	}

	@:op(A >>> B) private static inline function ushr(a:UInt, b:Int):UInt {
		return a.toInt() >>> b;
	}

	@:op(A % B) private static inline function mod(a:UInt, b:UInt):UInt {
		return Std.int( a.toFloat() % b.toFloat() );
	}

	@:commutative @:op(A + B) private static inline function addWithFloat(a:UInt, b:Float):Float {
		return a.toFloat() + b;
	}

	@:commutative @:op(A * B) private static inline function mulWithFloat(a:UInt, b:Float):Float {
		return a.toFloat() * b;
	}

	@:op(A / B) private static inline function divFloat(a:UInt, b:Float):Float {
		return a.toFloat() / b;
	}

	@:op(A / B) private static inline function floatDiv(a:Float, b:UInt):Float {
		return a / b.toFloat();
	}

	@:op(A - B) private static inline function subFloat(a:UInt, b:Float):Float {
		return a.toFloat() - b;
	}

	@:op(A - B) private static inline function floatSub(a:Float, b:UInt):Float {
		return a - b.toFloat();
	}

	@:op(A > B) private static inline function gtFloat(a:UInt, b:Float):Bool {
		return a.toFloat() > b;
	}

	@:commutative @:op(A == B) private static inline function equalsInt<T:Int>(a:UInt, b:T):Bool {
		return a.toInt() == b;
	}

	@:commutative @:op(A != B) private static inline function notEqualsInt<T:Int>(a:UInt, b:T):Bool {
		return a.toInt() != b;
	}

	@:commutative @:op(A == B) private static inline function equalsFloat<T:Float>(a:UInt, b:T):Bool {
		return a.toFloat() == b;
	}

	@:commutative @:op(A != B) private static inline function notEqualsFloat<T:Float>(a:UInt, b:T):Bool {
		return a.toFloat() != b;
	}

	@:op(A >= B) private static inline function gteFloat(a:UInt, b:Float):Bool {
		return a.toFloat() >= b;
	}


	@:op(A > B) private static inline function floatGt(a:Float, b:UInt):Bool {
		return a > b.toFloat();
	}

	@:op(A >= B) private static inline function floatGte(a:Float, b:UInt):Bool {
		return a >= b.toFloat();
	}

	@:op(A < B) private static inline function ltFloat(a:UInt, b:Float):Bool {
		return a.toFloat() < b;
	}

	@:op(A <= B) private static inline function lteFloat(a:UInt, b:Float):Bool {
		return a.toFloat() <= b;
	}

	@:op(A < B) private static inline function floatLt(a:Float, b:UInt):Bool {
		return a < b.toFloat();
	}

	@:op(A <= B) private static inline function floatLte(a:Float, b:UInt):Bool {
		return a <= b.toFloat();
	}

	@:op(A % B) private static inline function modFloat(a:UInt, b:Float):Float {
		return a.toFloat() % b;
	}

	@:op(A % B) private static inline function floatMod(a:Float, b:UInt):Float {
		return a % b.toFloat();
	}

	@:op(~A) private inline function negBits():UInt {
		return ~this;
	}

	@:op(++A) private inline function prefixIncrement():UInt {
		return ++this;
	}

	@:op(A++) private inline function postfixIncrement():UInt {
		return this++;
	}

	@:op(--A) private inline function prefixDecrement():UInt {
		return --this;
	}

	@:op(A--) private inline function postfixDecrement():UInt {
		return this--;
	}

	// TODO: radix is just defined to deal with doc_gen issues
	private inline function toString(?radix:Int):String {
		return Std.string(toFloat());
	}

	private inline function toInt():Int {
		return this;
	}

	@:to private #if (!js || analyzer) inline #end function toFloat():Float {
		var int = toInt();
		if (int < 0) {
			return 4294967296.0 + int;
		}
		else {
			// + 0.0 here to make sure we promote to Float on some platforms
			// In particular, PHP was having issues when comparing to Int in the == op.
			return int + 0.0;
		}
	}
}
#end
