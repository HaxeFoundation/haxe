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

package js.lib;

import js.Syntax;
import js.Lib.undefined;
import haxe.extern.EitherType;
import js.lib.intl.NumberFormat.NumberFormatOptions;

/**
	`BigInt` is a built-in object that provides a way to represent whole numbers larger than `2^53 - 1`,
	which is the largest number JavaScript can reliably represent with the `Number` primitive.
	`BigInt` can be used for arbitrarily large integers.

	Documentation [BigInt](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt>
**/
@:coreType
abstract BigInt {
	public inline function new(value:EitherType<String, Int>) {
		this = Syntax.code("BigInt({0})", value);
	}

	/**
		Returns a string with a language-sensitive representation of this number.
		Overrides the `Object.prototype.toLocaleString()` method.
	**/
	public inline function toLocaleString(?locales:EitherType<String, Array<String>>, ?options:NumberFormatOptions):String {
		return Syntax.code(
			"({0}).toLocaleString({1}, {2})",
			this,
			(locales != null) ? locales : undefined,
			(options != null) ? options : undefined
		);
	}

	/**
		Returns a string representing the specified object in the specified radix (base).
		Overrides the `Object.prototype.toString()` method.
	**/
	public inline function toString(?radix:Int):String {
		return Syntax.code(
			"({0}).toString({1})",
			this,
			(radix != null) ? radix : undefined
		);
	}

	/**
		Wraps a BigInt value to a signed integer between `-2^(width-1)` and `2^(width-1)-1`.
	**/
	public static inline function asIntN(width:Int, bigint:BigInt):BigInt {
		return Syntax.code("BigInt.asIntN({0}, {1})", width, bigint);
	}

	/**
		Wraps a BigInt value to an unsigned integer between `0` and `2^width-1`.
	**/
	public static inline function asUintN(width:Int, bigint:BigInt):BigInt {
		return Syntax.code("BigInt.asUintN({0}, {1})", width, bigint);
	}

	@:op(-A) static function neg(a:BigInt):BigInt;

	@:op(++A) static inline function incPre(a:BigInt):BigInt {
		return untyped __js__("++{0}", a);
	}

	@:op(A++) static inline function incPost(a:BigInt):BigInt {
		return untyped __js__("{0}++", a);
	}

	@:op(--A) static inline function decPre(a:BigInt):BigInt {
		return untyped __js__("--{0}", a);
	}

	@:op(A--) static inline function decPost(a:BigInt):BigInt {
		return untyped __js__("{0}--", a);
	}

	@:op(A + B) static function add(a:BigInt, b:BigInt):BigInt;

	@:op(A - B) static function sub(a:BigInt, b:BigInt):BigInt;

	@:op(A * B) static function mul(a:BigInt, b:BigInt):BigInt;

	@:op(A / B) static function div(a:BigInt, b:BigInt):BigInt;

	@:op(A % B) static function mod(a:BigInt, b:BigInt):BigInt;

	@:op(~A) static function not(a:BigInt):BigInt;

	@:op(A & B) static function and(a:BigInt, b:BigInt):BigInt;

	@:op(A | B) static function or(a:BigInt, b:BigInt):BigInt;

	@:op(A ^ B) static function xor(a:BigInt, b:BigInt):BigInt;

	@:op(A << B) static function lshift(a:BigInt, b:BigInt):BigInt;

	@:op(A >> B) static function rshift(a:BigInt, b:BigInt):BigInt;

	@:op(A == B) @:commutative static function eq<T:Float>(a:BigInt, b:EitherType<BigInt, T>):Bool;

	@:op(A != B) @:commutative static function neq<T:Float>(a:BigInt, b:EitherType<BigInt, T>):Bool;

	@:op(A < B) static function lt<T:Float>(a:EitherType<BigInt, T>, b:BigInt):Bool;

	@:op(A < B) static function lt2<T:Float>(a:BigInt, b:EitherType<BigInt, T>):Bool;

	@:op(A <= B) static function le<T:Float>(a:EitherType<BigInt, T>, b:BigInt):Bool;

	@:op(A <= B) static function le2<T:Float>(a:BigInt, b:EitherType<BigInt, T>):Bool;

	@:op(A > B) static function gt<T:Float>(a:EitherType<Int, BigInt>, b:BigInt):Bool;

	@:op(A > B) static function gt2<T:Float>(a:BigInt, b:EitherType<Int, BigInt>):Bool;

	@:op(A >= B) static function ge<T:Float>(a:EitherType<Int, BigInt>, b:BigInt):Bool;

	@:op(A >= B) static function ge2<T:Float>(a:BigInt, b:EitherType<Int, BigInt>):Bool;
}
