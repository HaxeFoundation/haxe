/*
 * Copyright (C)2005-2018 Haxe Foundation
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

@:coreApi
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

	@:op(A > B) private static function gt(a:UInt, b:UInt):Bool;

	@:op(A >= B) private static function gte(a:UInt, b:UInt):Bool;

	@:op(A < B) private static function lt(a:UInt, b:UInt):Bool;

	@:op(A <= B) private static function lte(a:UInt, b:UInt):Bool;

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

	@:op(A % B) private static function mod(a:UInt, b:UInt):UInt;

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

	@:commutative @:op(A == B) private static function equalsInt<T:Int>(a:UInt, b:T):Bool;
	@:commutative @:op(A != B) private static function notEqualsInt<T:Int>(a:UInt, b:T):Bool;
	@:commutative @:op(A == B) private static function equalsFloat<T:Float>(a:UInt, b:T):Bool;
    @:commutative @:op(A != B) private static function notEqualsFloat<T:Float>(a:UInt, b:T):Bool;

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

	@:to private inline function toFloat():Float {
		return cast (this:UInt);
	}
}

