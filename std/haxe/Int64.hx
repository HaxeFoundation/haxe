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

/**
	A cross-platform signed 64-bit integer.
	Int64 instances can be created from two 32-bit words using `Int64.make()`.

	This abstract defines the operator overloads and public API surface.
	The actual implementation is in `haxe.numeric.Int64Native`, which can be
	shadowed by platform-specific `_std` directories for native support.
**/
#if flash
@:notNull
#end
@:transitive
abstract Int64(__Int64) from __Int64 to __Int64 {
	private inline function new(x:__Int64)
		this = x;

	/**
		Makes a copy of `this` Int64.
	**/
	public inline function copy():Int64
		return make(high, low);

	/**
		Construct an Int64 from two 32-bit words `high` and `low`.
	**/
	public static inline function make(high:Int32, low:Int32):Int64
		return new Int64(__Int64.make(high, low));

	/**
		Returns an Int64 with the value of the Int `x`.
		`x` is sign-extended to fill 64 bits.
	**/
	@:from public static inline function ofInt(x:Int):Int64
		return new Int64(__Int64.ofInt(x));

	/**
		Returns an Int with the value of the Int64 `x`.
		Throws an exception  if `x` cannot be represented in 32 bits.
	**/
	public static inline function toInt(x:Int64):Int
		return __Int64.toInt(x);

	@:deprecated('haxe.Int64.is() is deprecated. Use haxe.Int64.isInt64() instead')
	inline public static function is(val:Dynamic):Bool {
		return isInt64(val);
	}

	/**
		Returns whether the value `val` is of type `haxe.Int64`
	**/
	inline public static function isInt64(val:Dynamic):Bool
		return Std.isOfType(val, __Int64);

	/**
		Returns the high 32-bit word of `x`.
	**/
	@:deprecated("Use high instead")
	public static inline function getHigh(x:Int64):Int32
		return x.high;

	/**
		Returns the low 32-bit word of `x`.
	**/
	@:deprecated("Use low instead")
	public static inline function getLow(x:Int64):Int32
		return x.low;

	/**
		Returns `true` if `x` is less than zero.
	**/
	public static inline function isNeg(x:Int64):Bool
		return __Int64.isNeg(x);

	/**
		Returns `true` if `x` is exactly zero.
	**/
	public static inline function isZero(x:Int64):Bool
		return __Int64.isZero(x);

	/**
		Compares `a` and `b` in signed mode.
		Returns a negative value if `a < b`, positive if `a > b`,
		or 0 if `a == b`.
	**/
	public static inline function compare(a:Int64, b:Int64):Int
		return __Int64.compare(a, b);

	/**
		Compares `a` and `b` in unsigned mode.
		Returns a negative value if `a < b`, positive if `a > b`,
		or 0 if `a == b`.
	**/
	public static inline function ucompare(a:Int64, b:Int64):Int
		return __Int64.ucompare(a, b);

	/**
		Returns a signed decimal `String` representation of `x`.
	**/
	public static inline function toStr(x:Int64):String
		return x.toString();

	public inline function toString():String
		return this.toString();

	public static inline function parseString(sParam:String):Int64 {
		return Int64Helper.parseString(sParam);
	}

	public static inline function fromFloat(f:Float):Int64 {
		return Int64Helper.fromFloat(f);
	}

	/**
		Performs signed integer division of `dividend` by `divisor`.
		Returns `{ quotient : Int64, modulus : Int64 }`.
	**/
	public static function divMod(dividend:Int64, divisor:Int64):{quotient:Int64, modulus:Int64} {
		var r = __Int64.divMod(dividend, divisor);
		return {quotient: r.quotient, modulus: r.modulus};
	}

	/**
		Returns the negative of `x`.
	**/
	@:op(-A) public static inline function neg(x:Int64):Int64
		return __Int64.neg(x);

	@:op(++A) private inline function preIncrement():Int64 {
		this = __Int64.add(this, __Int64.ofInt(1));
		return cast this;
	}

	@:op(A++) private inline function postIncrement():Int64 {
		var ret = this;
		preIncrement();
		return ret;
	}

	@:op(--A) private inline function preDecrement():Int64 {
		this = __Int64.sub(this, __Int64.ofInt(1));
		return cast this;
	}

	@:op(A--) private inline function postDecrement():Int64 {
		var ret = this;
		preDecrement();
		return ret;
	}

	/**
		Returns the sum of `a` and `b`.
	**/
	@:op(A + B) public static inline function add(a:Int64, b:Int64):Int64
		return __Int64.add(a, b);

	@:op(A + B) @:commutative private static inline function addInt(a:Int64, b:Int):Int64
		return add(a, b);

	/**
		Returns `a` minus `b`.
	**/
	@:op(A - B) public static inline function sub(a:Int64, b:Int64):Int64
		return __Int64.sub(a, b);

	@:op(A - B) private static inline function subInt(a:Int64, b:Int):Int64
		return sub(a, b);

	@:op(A - B) private static inline function intSub(a:Int, b:Int64):Int64
		return sub(a, b);

	/**
		Returns the product of `a` and `b`.
	**/
	@:op(A * B) public static inline function mul(a:Int64, b:Int64):Int64
		return __Int64.mul(a, b);

	@:op(A * B) @:commutative private static inline function mulInt(a:Int64, b:Int):Int64
		return mul(a, b);

	/**
		Returns the quotient of `a` divided by `b`.
	**/
	@:op(A / B) public static inline function div(a:Int64, b:Int64):Int64
		return divMod(a, b).quotient;

	@:op(A / B) private static inline function divInt(a:Int64, b:Int):Int64
		return div(a, b);

	@:op(A / B) private static inline function intDiv(a:Int, b:Int64):Int64
		return toInt(div(a, b));

	/**
		Returns the modulus of `a` divided by `b`.
	**/
	@:op(A % B) public static inline function mod(a:Int64, b:Int64):Int64
		return divMod(a, b).modulus;

	@:op(A % B) private static inline function modInt(a:Int64, b:Int):Int64
		return toInt(mod(a, b));

	@:op(A % B) private static inline function intMod(a:Int, b:Int64):Int64
		return toInt(mod(a, b));

	/**
		Returns `true` if `a` is equal to `b`.
	**/
	@:op(A == B) public static inline function eq(a:Int64, b:Int64):Bool
		return __Int64.eq(a, b);

	@:op(A == B) @:commutative private static inline function eqInt(a:Int64, b:Int):Bool
		return eq(a, b);

	/**
		Returns `true` if `a` is not equal to `b`.
	**/
	@:op(A != B) public static inline function neq(a:Int64, b:Int64):Bool
		return __Int64.neq(a, b);

	@:op(A != B) @:commutative private static inline function neqInt(a:Int64, b:Int):Bool
		return neq(a, b);

	@:op(A < B) private static inline function lt(a:Int64, b:Int64):Bool
		return compare(a, b) < 0;

	@:op(A < B) private static inline function ltInt(a:Int64, b:Int):Bool
		return lt(a, b);

	@:op(A < B) private static inline function intLt(a:Int, b:Int64):Bool
		return lt(a, b);

	@:op(A <= B) private static inline function lte(a:Int64, b:Int64):Bool
		return compare(a, b) <= 0;

	@:op(A <= B) private static inline function lteInt(a:Int64, b:Int):Bool
		return lte(a, b);

	@:op(A <= B) private static inline function intLte(a:Int, b:Int64):Bool
		return lte(a, b);

	@:op(A > B) private static inline function gt(a:Int64, b:Int64):Bool
		return compare(a, b) > 0;

	@:op(A > B) private static inline function gtInt(a:Int64, b:Int):Bool
		return gt(a, b);

	@:op(A > B) private static inline function intGt(a:Int, b:Int64):Bool
		return gt(a, b);

	@:op(A >= B) private static inline function gte(a:Int64, b:Int64):Bool
		return compare(a, b) >= 0;

	@:op(A >= B) private static inline function gteInt(a:Int64, b:Int):Bool
		return gte(a, b);

	@:op(A >= B) private static inline function intGte(a:Int, b:Int64):Bool
		return gte(a, b);

	/**
		Returns the bitwise NOT of `a`.
	**/
	@:op(~A) private static inline function complement(a:Int64):Int64
		return __Int64.complement(a);

	/**
		Returns the bitwise AND of `a` and `b`.
	**/
	@:op(A & B) public static inline function and(a:Int64, b:Int64):Int64
		return __Int64.and(a, b);

	/**
		Returns the bitwise OR of `a` and `b`.
	**/
	@:op(A | B) public static inline function or(a:Int64, b:Int64):Int64
		return __Int64.or(a, b);

	/**
		Returns the bitwise XOR of `a` and `b`.
	**/
	@:op(A ^ B) public static inline function xor(a:Int64, b:Int64):Int64
		return __Int64.xor(a, b);

	/**
		Returns `a` left-shifted by `b` bits.
	**/
	@:op(A << B) public static inline function shl(a:Int64, b:Int):Int64
		return __Int64.shl(a, b);

	/**
		Returns `a` right-shifted by `b` bits in signed mode.
		`a` is sign-extended.
	**/
	@:op(A >> B) public static inline function shr(a:Int64, b:Int):Int64
		return __Int64.shr(a, b);

	/**
		Returns `a` right-shifted by `b` bits in unsigned mode.
		`a` is padded with zeroes.
	**/
	@:op(A >>> B) public static inline function ushr(a:Int64, b:Int):Int64
		return __Int64.ushr(a, b);

	public var high(get, never):Int32;

	private inline function get_high()
		return this.high;

	private inline function set_high(x)
		return this.high = x;

	public var low(get, never):Int32;

	private inline function get_low()
		return this.low;

	private inline function set_low(x)
		return this.low = x;

	static var IMPL = haxe.numeric.Int64Native;
}

/**
	This typedef will fool `@:coreApi` into thinking that we are using
	the same underlying type, even though it might be different on
	specific platforms.
**/
private typedef __Int64 = haxe.numeric.Int64Native;
