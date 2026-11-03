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

#if ((flash || flash9doc || hl) && !doc_gen)
/**
	The unsigned `Int` type is only defined for Flash. It's currently
	handled the same as a normal Int.

	@see https://haxe.org/manual/types-basic-types.html
**/
@:coreType
@:notNull
@:runtimeValue
@:analyzer(no_const_propagation)
abstract UInt32 to Int from Int {
	@:commutative @:op(A + B) private static function addI(lhs:UInt32, rhs:Int):UInt32;

	@:commutative @:op(A + B) private static function addF(lhs:UInt32, rhs:Float):Float;

	@:op(A + B) private static function add(lhs:UInt32, rhs:UInt32):UInt32;

	@:commutative @:op(A * B) private static function mulI(lhs:UInt32, rhs:Int):UInt32;

	@:commutative @:op(A * B) private static function mulF(lhs:UInt32, rhs:Float):Float;

	@:op(A * B) private static function mul(lhs:UInt32, rhs:UInt32):UInt32;

	@:op(A % B) private static function modI(lhs:UInt32, rhs:Int):UInt32;

	@:op(A % B) private static function modF(lhs:UInt32, rhs:Float):Float;

	@:op(A % B) private static function mod(lhs:UInt32, rhs:UInt32):UInt32;

	@:op(A - B) private static function subI(lhs:UInt32, rhs:Int):UInt32;

	@:op(A - B) private static function subF(lhs:UInt32, rhs:Float):Float;

	@:op(A - B) private static function sub(lhs:UInt32, rhs:UInt32):UInt32;

	@:op(A / B) private static function divI(lhs:UInt32, rhs:Int):Float;

	@:op(A / B) private static function divF(lhs:UInt32, rhs:Float):Float;

	@:op(A / B) private static function div(lhs:UInt32, rhs:UInt32):Float;

	@:commutative @:op(A | B) private static function orI(lhs:UInt32, rhs:Int):UInt32;

	@:op(A | B) private static function or(lhs:UInt32, rhs:UInt32):UInt32;

	@:commutative @:op(A ^ B) private static function xorI(lhs:UInt32, rhs:Int):UInt32;

	@:op(A ^ B) private static function xor(lhs:UInt32, rhs:UInt32):UInt32;

	@:commutative @:op(A & B) private static function andI(lhs:UInt32, rhs:Int):UInt32;

	@:op(A & B) private static function and(lhs:UInt32, rhs:UInt32):UInt32;

	@:op(A << B) private static function shl(lhs:UInt32, rhs:Int):UInt32;

	@:op(A >> B) private static inline function shr(lhs:UInt32, rhs:Int):UInt32
		return lhs >>> rhs;

	@:op(A >>> B) private static function ushr(lhs:UInt32, rhs:Int):UInt32;

	@:op(A > B) private static function gt(lhs:UInt32, rhs:UInt32):Bool;

	@:op(A >= B) private static function gte(lhs:UInt32, rhs:UInt32):Bool;

	@:op(A < B) private static function lt(lhs:UInt32, rhs:UInt32):Bool;

	@:op(A <= B) private static function lte(lhs:UInt32, rhs:UInt32):Bool;

	@:op(A > B) private static function gtf(lhs:UInt32, rhs:Float):Bool;

	@:op(A > B) private static function gtf2(lhs:Float, rhs:UInt32):Bool;

	@:op(A >= B) private static function gtef(lhs:UInt32, rhs:Float):Bool;

	@:op(A >= B) private static function gtef2(lhs:Float, rhs:UInt32):Bool;

	@:op(A < B) private static function ltf(lhs:UInt32, rhs:Float):Bool;

	@:op(A < B) private static function ltf2(lhs:Float, rhs:UInt32):Bool;

	@:op(A <= B) private static function ltef(lhs:UInt32, rhs:Float):Bool;

	@:op(A <= B) private static function ltef2(lhs:Float, rhs:UInt32):Bool;

	@:op(~A) private static function bneg(t:UInt32):UInt32;

	@:commutative @:op(A == B) private static function equalsInt<T:Int>(a:UInt32, b:T):Bool;

	@:commutative @:op(A != B) private static function notEqualsInt<T:Int>(a:UInt32, b:T):Bool;

	@:commutative @:op(A == B) private static function equalsFloat<T:Float>(a:UInt32, b:T):Bool;

	@:commutative @:op(A != B) private static function notEqualsFloat<T:Float>(a:UInt32, b:T):Bool;

	@:op(++A) private function prefixIncrement():UInt32;

	@:op(A++) private function postfixIncrement():UInt32;

	@:op(--A) private function prefixDecrement():UInt32;

	@:op(A--) private function postfixDecrement():UInt32;
}
#else

/**
	The unsigned `Int` type is only defined for Flash.
	Simulate it for other platforms.

	@see https://haxe.org/manual/types-basic-types.html
**/
@:transitive
abstract UInt32(Int) from Int to Int {
	@:op(A + B) private static inline function add(a:UInt32, b:UInt32):UInt32 {
		return a.toInt() + b.toInt();
	}

	@:op(A / B) private static inline function div(a:UInt32, b:UInt32):Float {
		return a.toFloat() / b.toFloat();
	}

	@:op(A * B) private static inline function mul(a:UInt32, b:UInt32):UInt32 {
		return a.toInt() * b.toInt();
	}

	@:op(A - B) private static inline function sub(a:UInt32, b:UInt32):UInt32 {
		return a.toInt() - b.toInt();
	}

	@:op(A > B)
	private static #if !js inline #end function gt(a:UInt32, b:UInt32):Bool {
		var aNeg = a.toInt() < 0;
		var bNeg = b.toInt() < 0;
		return if (aNeg != bNeg) aNeg; else a.toInt() > b.toInt();
	}

	@:op(A >= B)
	private static #if !js inline #end function gte(a:UInt32, b:UInt32):Bool {
		var aNeg = a.toInt() < 0;
		var bNeg = b.toInt() < 0;
		return if (aNeg != bNeg) aNeg; else a.toInt() >= b.toInt();
	}

	@:op(A < B) private static inline function lt(a:UInt32, b:UInt32):Bool {
		return gt(b, a);
	}

	@:op(A <= B) private static inline function lte(a:UInt32, b:UInt32):Bool {
		return gte(b, a);
	}

	@:op(A & B) private static inline function and(a:UInt32, b:UInt32):UInt32 {
		return a.toInt() & b.toInt();
	}

	@:op(A | B) private static inline function or(a:UInt32, b:UInt32):UInt32 {
		return a.toInt() | b.toInt();
	}

	@:op(A ^ B) private static inline function xor(a:UInt32, b:UInt32):UInt32 {
		return a.toInt() ^ b.toInt();
	}

	@:op(A << B) private static inline function shl(a:UInt32, b:Int):UInt32 {
		return a.toInt() << b;
	}

	@:op(A >> B) private static inline function shr(a:UInt32, b:Int):UInt32 {
		return a.toInt() >>> b;
	}

	@:op(A >>> B) private static inline function ushr(a:UInt32, b:Int):UInt32 {
		return a.toInt() >>> b;
	}

	@:op(A % B) private static inline function mod(a:UInt32, b:UInt32):UInt32 {
		return Std.int(a.toFloat() % b.toFloat());
	}

	@:commutative @:op(A + B) private static inline function addWithFloat(a:UInt32, b:Float):Float {
		return a.toFloat() + b;
	}

	@:commutative @:op(A * B) private static inline function mulWithFloat(a:UInt32, b:Float):Float {
		return a.toFloat() * b;
	}

	@:op(A / B) private static inline function divFloat(a:UInt32, b:Float):Float {
		return a.toFloat() / b;
	}

	@:op(A / B) private static inline function floatDiv(a:Float, b:UInt32):Float {
		return a / b.toFloat();
	}

	@:op(A - B) private static inline function subFloat(a:UInt32, b:Float):Float {
		return a.toFloat() - b;
	}

	@:op(A - B) private static inline function floatSub(a:Float, b:UInt32):Float {
		return a - b.toFloat();
	}

	@:op(A > B) private static inline function gtFloat(a:UInt32, b:Float):Bool {
		return a.toFloat() > b;
	}

	@:commutative @:op(A == B) private static inline function equalsInt<T:Int>(a:UInt32, b:T):Bool {
		return a.toInt() == b;
	}

	@:commutative @:op(A != B) private static inline function notEqualsInt<T:Int>(a:UInt32, b:T):Bool {
		return a.toInt() != b;
	}

	@:commutative @:op(A == B) private static inline function equalsFloat<T:Float>(a:UInt32, b:T):Bool {
		return a.toFloat() == b;
	}

	@:commutative @:op(A != B) private static inline function notEqualsFloat<T:Float>(a:UInt32, b:T):Bool {
		return a.toFloat() != b;
	}

	@:op(A >= B) private static inline function gteFloat(a:UInt32, b:Float):Bool {
		return a.toFloat() >= b;
	}

	@:op(A > B) private static inline function floatGt(a:Float, b:UInt32):Bool {
		return a > b.toFloat();
	}

	@:op(A >= B) private static inline function floatGte(a:Float, b:UInt32):Bool {
		return a >= b.toFloat();
	}

	@:op(A < B) private static inline function ltFloat(a:UInt32, b:Float):Bool {
		return a.toFloat() < b;
	}

	@:op(A <= B) private static inline function lteFloat(a:UInt32, b:Float):Bool {
		return a.toFloat() <= b;
	}

	@:op(A < B) private static inline function floatLt(a:Float, b:UInt32):Bool {
		return a < b.toFloat();
	}

	@:op(A <= B) private static inline function floatLte(a:Float, b:UInt32):Bool {
		return a <= b.toFloat();
	}

	@:op(A % B) private static inline function modFloat(a:UInt32, b:Float):Float {
		return a.toFloat() % b;
	}

	@:op(A % B) private static inline function floatMod(a:Float, b:UInt32):Float {
		return a % b.toFloat();
	}

	@:op(~A) private inline function negBits():UInt32 {
		return ~this;
	}

	@:op(++A) private inline function prefixIncrement():UInt32 {
		return ++this;
	}

	@:op(A++) private inline function postfixIncrement():UInt32 {
		return this++;
	}

	@:op(--A) private inline function prefixDecrement():UInt32 {
		return --this;
	}

	@:op(A--) private inline function postfixDecrement():UInt32 {
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
		} else {
			// + 0.0 here to make sure we promote to Float on some platforms
			// In particular, PHP was having issues when comparing to Int in the == op.
			return int + 0.0;
		}
	}
}
#end
