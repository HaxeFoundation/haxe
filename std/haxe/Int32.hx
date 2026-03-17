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

import haxe.numeric.Int32Native;

/**
	A cross-platform signed 32-bit integer with consistent overflow behavior.

	This abstract defines the operator overloads and public API surface.
	The actual implementation is in `haxe.numeric.Int32Native`, which can be
	shadowed by platform-specific `_std` directories for native support.
**/
@:transitive
abstract Int32(Int32Native) from Int to Int {
	private inline function new(x:Int32Native)
		this = x;

	@:op(-A) private static inline function neg(x:Int32):Int32
		return Int32Native.neg(x);

	@:op(++A) private inline function preIncrement():Int32 {
		this = Int32Native.add(this, 1);
		return cast this;
	}

	@:op(A++) private inline function postIncrement():Int32 {
		var ret = this;
		this = Int32Native.add(this, 1);
		return ret;
	}

	@:op(--A) private inline function preDecrement():Int32 {
		this = Int32Native.sub(this, 1);
		return cast this;
	}

	@:op(A--) private inline function postDecrement():Int32 {
		var ret = this;
		this = Int32Native.sub(this, 1);
		return ret;
	}

	@:op(A + B) private static inline function add(a:Int32, b:Int32):Int32
		return Int32Native.add(a, b);

	@:op(A + B) @:commutative private static inline function addInt(a:Int32, b:Int):Int32
		return Int32Native.add(a, b);

	@:op(A + B) @:commutative private static function addFloat(a:Int32, b:Float):Float;

	@:op(A - B) private static inline function sub(a:Int32, b:Int32):Int32
		return Int32Native.sub(a, b);

	@:op(A - B) private static inline function subInt(a:Int32, b:Int):Int32
		return Int32Native.sub(a, b);

	@:op(A - B) private static inline function intSub(a:Int, b:Int32):Int32
		return Int32Native.sub(a, b);

	@:op(A - B) private static function subFloat(a:Int32, b:Float):Float;

	@:op(A - B) private static function floatSub(a:Float, b:Int32):Float;

	@:op(A * B) private static inline function mul(a:Int32, b:Int32):Int32
		return Int32Native.mul(a, b);

	@:op(A * B) @:commutative private static inline function mulInt(a:Int32, b:Int):Int32
		return Int32Native.mul(a, b);

	@:op(A * B) @:commutative private static function mulFloat(a:Int32, b:Float):Float;

	@:op(A / B) private static function div(a:Int32, b:Int32):Float;

	@:op(A / B) private static function divInt(a:Int32, b:Int):Float;

	@:op(A / B) private static function intDiv(a:Int, b:Int32):Float;

	@:op(A / B) private static function divFloat(a:Int32, b:Float):Float;

	@:op(A / B) private static function floatDiv(a:Float, b:Int32):Float;

	@:op(A % B) private static function mod(a:Int32, b:Int32):Int32;

	@:op(A % B) private static function modInt(a:Int32, b:Int):Int;

	@:op(A % B) private static function intMod(a:Int, b:Int32):Int;

	@:op(A % B) private static function modFloat(a:Int32, b:Float):Float;

	@:op(A % B) private static function floatMod(a:Float, b:Int32):Float;

	@:op(A == B) private static function eq(a:Int32, b:Int32):Bool;

	@:op(A == B) @:commutative private static function eqInt(a:Int32, b:Int):Bool;

	@:op(A == B) @:commutative private static function eqFloat(a:Int32, b:Float):Bool;

	@:op(A != B) private static function neq(a:Int32, b:Int32):Bool;

	@:op(A != B) @:commutative private static function neqInt(a:Int32, b:Int):Bool;

	@:op(A != B) @:commutative private static function neqFloat(a:Int32, b:Float):Bool;

	@:op(A < B) private static function lt(a:Int32, b:Int32):Bool;

	@:op(A < B) private static function ltInt(a:Int32, b:Int):Bool;

	@:op(A < B) private static function intLt(a:Int, b:Int32):Bool;

	@:op(A < B) private static function ltFloat(a:Int32, b:Float):Bool;

	@:op(A < B) private static function floatLt(a:Float, b:Int32):Bool;

	@:op(A <= B) private static function lte(a:Int32, b:Int32):Bool;

	@:op(A <= B) private static function lteInt(a:Int32, b:Int):Bool;

	@:op(A <= B) private static function intLte(a:Int, b:Int32):Bool;

	@:op(A <= B) private static function lteFloat(a:Int32, b:Float):Bool;

	@:op(A <= B) private static function floatLte(a:Float, b:Int32):Bool;

	@:op(A > B) private static function gt(a:Int32, b:Int32):Bool;

	@:op(A > B) private static function gtInt(a:Int32, b:Int):Bool;

	@:op(A > B) private static function intGt(a:Int, b:Int32):Bool;

	@:op(A > B) private static function gtFloat(a:Int32, b:Float):Bool;

	@:op(A > B) private static function floatGt(a:Float, b:Int32):Bool;

	@:op(A >= B) private static function gte(a:Int32, b:Int32):Bool;

	@:op(A >= B) private static function gteInt(a:Int32, b:Int):Bool;

	@:op(A >= B) private static function intGte(a:Int, b:Int32):Bool;

	@:op(A >= B) private static function gteFloat(a:Int32, b:Float):Bool;

	@:op(A >= B) private static function floatGte(a:Float, b:Int32):Bool;

	@:op(~A) private static inline function complement(a:Int32):Int32
		return Int32Native.complement(a);

	@:op(A & B) private static inline function and(a:Int32, b:Int32):Int32
		return Int32Native.and(a, b);

	@:op(A & B) @:commutative private static inline function andInt(a:Int32, b:Int):Int32
		return Int32Native.and(a, b);

	@:op(A | B) private static inline function or(a:Int32, b:Int32):Int32
		return Int32Native.or(a, b);

	@:op(A | B) @:commutative private static inline function orInt(a:Int32, b:Int):Int32
		return Int32Native.or(a, b);

	@:op(A ^ B) private static inline function xor(a:Int32, b:Int32):Int32
		return Int32Native.xor(a, b);

	@:op(A ^ B) @:commutative private static inline function xorInt(a:Int32, b:Int):Int32
		return Int32Native.xor(a, b);

	@:op(A >> B) private static inline function shr(a:Int32, b:Int32):Int32
		return Int32Native.shr(a, (b : Int));

	@:op(A >> B) private static inline function shrInt(a:Int32, b:Int):Int32
		return Int32Native.shr(a, b);

	@:op(A >> B) private static inline function intShr(a:Int, b:Int32):Int32
		return Int32Native.shr(a, (b : Int));

	@:op(A >>> B) private static inline function ushr(a:Int32, b:Int32):Int32
		return Int32Native.ushr(a, (b : Int));

	@:op(A >>> B) private static inline function ushrInt(a:Int32, b:Int):Int32
		return Int32Native.ushr(a, b);

	@:op(A >>> B) private static inline function intUshr(a:Int, b:Int32):Int32
		return Int32Native.ushr(a, (b : Int));

	@:op(A << B) private static inline function shl(a:Int32, b:Int32):Int32
		return Int32Native.shl(a, (b : Int));

	@:op(A << B) private static inline function shlInt(a:Int32, b:Int):Int32
		return Int32Native.shl(a, b);

	@:op(A << B) private static inline function intShl(a:Int, b:Int32):Int32
		return Int32Native.shl(a, (b : Int));

	@:to private inline function toFloat():Float
		return (this : Int);

	/**
		Compare `a` and `b` in unsigned mode.
	**/
	public static inline function ucompare(a:Int32, b:Int32):Int
		return Int32Native.ucompare(a, b);
}
