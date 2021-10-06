/*
 * Copyright (C)2005-2021 Haxe Foundation
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

@:transitive
abstract UInt64(Int64) from Int64 to Int64 {
	@:op(A + B) public static function add(lhs:UInt64, rhs:UInt64):UInt64
		return Int64.add(lhs, rhs);

	@:commutative @:op(A + B) private static function addI(lhs:UInt64, rhs:Int):UInt64;

	@:commutative @:op(A + B) private static function addF(lhs:UInt64, rhs:Float):Float;

	@:op(A * B) public static function mul(lhs:UInt64, rhs:UInt64):UInt64
		return Int64.mul(lhs, rhs);

	@:commutative @:op(A * B) private static function mulInt(lhs:UInt64, rhs:Int):UInt64;

	@:commutative @:op(A * B) private static function mulFloat(lhs:UInt64, rhs:Float):Float;

	@:op(A % B) public static function mod(lhs:UInt64, rhs:UInt64):UInt64
		return Int64.mod(lhs, rhs);

	@:op(A % B) private static function modI(lhs:UInt64, rhs:Int):UInt64;

	@:op(A % B) private static function modF(lhs:UInt64, rhs:Float):Float;

	@:op(A - B) private static function sub(lhs:UInt64, rhs:UInt64):UInt64;

	@:op(A - B) private static function subI(lhs:UInt64, rhs:Int):UInt64;

	@:op(A - B) private static function subF(lhs:UInt64, rhs:Float):Float;

	@:op(A / B) private static function div(lhs:UInt64, rhs:UInt64):Float;

	@:op(A / B) private static function divI(lhs:UInt64, rhs:Int):Float;

	@:op(A / B) private static function divF(lhs:UInt64, rhs:Float):Float;

	@:op(A | B) private static function or(lhs:UInt64, rhs:UInt64):UInt64;

	@:commutative @:op(A | B) private static function orI(lhs:UInt64, rhs:Int):UInt64;

	@:op(A ^ B) private static function xor(lhs:UInt64, rhs:UInt64):UInt64;

	@:commutative @:op(A ^ B) private static function xorI(lhs:UInt64, rhs:Int):UInt64;

	@:op(A & B) private static function and(lhs:UInt64, rhs:UInt64):UInt64;

	@:commutative @:op(A & B) private static function andI(lhs:UInt64, rhs:Int):UInt64;

	@:op(A << B) private static function shl(lhs:UInt64, rhs:Int):UInt64;

	@:op(A >> B) private static function shr(lhs:UInt64, rhs:Int):UInt64;

	@:op(A >>> B) private static function ushr(lhs:UInt64, rhs:Int):UInt64;

	@:op(A > B) private static function gt(lhs:UInt64, rhs:UInt64):Bool;

	@:op(A >= B) private static function gte(lhs:UInt64, rhs:UInt64):Bool;

	@:op(A < B) private static function lt(lhs:UInt64, rhs:UInt64):Bool;

	@:op(A <= B) private static function lte(lhs:UInt64, rhs:UInt64):Bool;

	@:op(A > B) private static function gtf(lhs:UInt64, rhs:Float):Bool;

	@:op(A > B) private static function gtf2(lhs:Float, rhs:UInt64):Bool;

	@:op(A >= B) private static function gtef(lhs:UInt64, rhs:Float):Bool;

	@:op(A >= B) private static function gtef2(lhs:Float, rhs:UInt64):Bool;

	@:op(A < B) private static function ltf(lhs:UInt64, rhs:Float):Bool;

	@:op(A < B) private static function ltf2(lhs:Float, rhs:UInt64):Bool;

	@:op(A <= B) private static function ltef(lhs:UInt64, rhs:Float):Bool;

	@:op(A <= B) private static function ltef2(lhs:Float, rhs:UInt64):Bool;

	@:op(~A) private static function bneg(t:UInt64):UInt64;

	@:commutative @:op(A == B) private static function equalsInt<T:Int>(a:UInt64, b:T):Bool;

	@:commutative @:op(A != B) private static function notEqualsInt<T:Int>(a:UInt64, b:T):Bool;

	@:commutative @:op(A == B) private static function equalsFloat<T:Float>(a:UInt64, b:T):Bool;

	@:commutative @:op(A != B) private static function notEqualsFloat<T:Float>(a:UInt64, b:T):Bool;

	@:op(++A) private inline function prefixIncrement():UInt64
		return @:privateAccess this.preIncrement();

	@:op(A++) private inline function postfixIncrement():UInt64
		return @:privateAccess this.postIncrement();

	@:op(--A) private inline function prefixDecrement():UInt64
		return @:privateAccess this.preDecrement();

	@:op(A--) private inline function postfixDecrement():UInt64
		return @:privateAccess this.postDecrement();

	function toString():String {
		var i:Int64 = cast this;
		if (i == 0)
			return "0";
		var str = "";
		var neg = false;
		if (Int64.isNeg(i)) {
			neg = true;
			i++;
			// i = -i; cannot negate here as --9223372036854775808 = -9223372036854775808
		}
		var ten:Int64 = 10;
		while (i != 0) {
			var r = Int64.divMod(i, ten);
			if (Int64.isNeg(r.modulus)) {
				str = Int64.neg(r.modulus).low + str;
				i = Int64.neg(r.quotient);
			} else {
				str = r.modulus.low + str;
				i = r.quotient;
			}
		}
		if (neg) {
			var newStr = "18446744073709551615";
			var sumStr = "";
			var subtract = 0;
			for (i in 0...newStr.length - str.length)
				str = "0" + str;
			final len = str.length;
			for (i in 0...len) { // from right to left
				final digitInt = str.charCodeAt(len - 1 - i) - '0'.code;
				final digitIntMax = newStr.charCodeAt(len - 1 - i) - '0'.code;
				if (digitInt < 0 || digitInt > 9) {
					throw "NumberFormatError";
				}
				var sum = (digitIntMax - subtract) - digitInt;
				subtract = 0;
				if (sum < 0) {
					sum += 10;
					subtract++;
				}
				sumStr = sum + sumStr;
			}
			for (i in 0...sumStr.length) {
				if (sumStr.charCodeAt(i) != '0'.code)
					return sumStr.substr(i);
			}
			return sumStr;
		}
		return str;
	}

	private inline function toInt():Int {
		return Int64.toInt(this);
	}

	public static inline function parseString(sParam:String):UInt64 {
		return UInt64Helper.parseString(sParam);
	}

	@:from public static inline function ofInt(x:Int):UInt64
		return Int64.ofInt(x);

	@:to private #if (!js || analyzer) inline #end function toFloat():Float {
		var int = toInt();
		if (int < 0) {
			return 18446744073709551616.0 + int;
		} else {
			// + 0.0 here to make sure we promote to Float on some platforms
			// In particular, PHP was having issues when comparing to Int in the == op.
			return int + 0.0;
		}
	}
}
