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
import haxe.extern.EitherType;
import js.lib.intl.NumberFormat.NumberFormatOptions;

/**
	`BigInt` is a built-in object that provides a way to represent whole numbers larger than `2^53 - 1`,
	which is the largest number JavaScript can reliably represent with the `Number` primitive.
	`BigInt` can be used for arbitrarily large integers.
**/
abstract BigInt(_BigInt) from _BigInt to _BigInt {
	public inline function new(value:EitherType<String, Int>) {
		this = new _BigInt(value);
	}

	/**
		Returns a string with a language-sensitive representation of this number.
		Overrides the `Object.prototype.toLocaleString()` method.
	**/
	public inline function toLocaleString(?locales:EitherType<String, Array<String>>, ?options:NumberFormatOptions):String {
		return if (locales == null) {
			if (options == null) {
				this.toLocaleString();
			} else {
				this.toLocaleString(js.Lib.undefined, options);
			}
		} else {
			if (options == null) {
				this.toLocaleString(locales);
			} else {
				this.toLocaleString(locales, options);
			}
		}
	}

	/**
		Returns a string representing the specified object in the specified radix (base).
		Overrides the `Object.prototype.toString()` method.
	**/
	public inline function toString(?radix:Int):String {
		return if (radix == null) {
			this.toString();
		} else {
			this.toString(radix);
		}
	}

	/**
		Returns the primitive value of the specified object.
		Overrides the `Object.prototype.valueOf()` method.
	**/
	public inline function valueOf():BigInt {
		return this.valueOf();
	}

	/**
		Wraps a BigInt value to a signed integer between `-2^(width-1)` and `2^(width-1-1)`.
	**/
	public static inline function asIntN(width:Int, bigint:BigInt):BigInt {
		return _BigInt.asIntN(width, bigint);
	}

	/**
		Wraps a BigInt value to an unsigned integer between `0` and `2^width-1`.
	**/
	public static inline function asUintN(width:Int, bigint:BigInt):BigInt {
		return _BigInt.asUintN(width, bigint);
	}

	@:op(-A) static inline function neg(a:BigInt):BigInt {
		return (Syntax.code("(-{0})", a));
	}

	@:op(++A) static inline function incPre(a:BigInt):BigInt {
		return Syntax.code("(++{0})", a);
	}

	@:op(A++) static inline function incPost(a:BigInt):BigInt {
		return Syntax.code("({0}++)", a);
	}

	@:op(--A) static inline function decPre(a:BigInt):BigInt {
		return Syntax.code("(--{0})", a);
	}

	@:op(A--) static inline function decPost(a:BigInt):BigInt {
		return Syntax.code("({0}--)", a);
	}

	@:op(A + B) static inline function add(a:BigInt, b:BigInt):BigInt {
		return Syntax.code("({0} + {1})", a, b);
	}

	@:op(A - B) static inline function sub(a:BigInt, b:BigInt):BigInt {
		return Syntax.code("({0} - {1})", a, b);
	}

	@:op(A * B) static inline function mul(a:BigInt, b:BigInt):BigInt {
		return Syntax.code("({0} * {1})", a, b);
	}

	@:op(A / B) static inline function div(a:BigInt, b:BigInt):BigInt {
		return Syntax.code("({0} / {1})", a, b);
	}

	@:op(A % B) static inline function mod(a:BigInt, b:BigInt):BigInt {
		return Syntax.code("({0} % {1})", a, b);
	}

	@:op(~A) static inline function not(a:BigInt):BigInt {
		return Syntax.code("~{0}", a);
	}

	@:op(A & B) static inline function and(a:BigInt, b:BigInt):BigInt {
		return Syntax.code("({0} & {1})", a, b);
	}

	@:op(A | B) static inline function or(a:BigInt, b:BigInt):BigInt {
		return Syntax.code("({0} | {1})", a, b);
	}

	@:op(A ^ B) static inline function xor(a:BigInt, b:BigInt):BigInt {
		return Syntax.code("({0} ^ {1})", a, b);
	}

	@:op(A << B) static inline function lshift(a:BigInt, b:BigInt):BigInt {
		return Syntax.code("({0} << {1})", a, b);
	}

	@:op(A >> B) static inline function rshift(a:BigInt, b:BigInt):BigInt {
		return Syntax.code("({0} >> {1})", a, b);
	}

	@:op(A == B) @:commutative static inline function eq(a:BigInt, b:EitherType<Int, BigInt>):Bool {
		return Syntax.code("({0} == {1})", a, b);
	}

	@:op(A != B) @:commutative static inline function neq(a:BigInt, b:EitherType<Int, BigInt>):Bool {
		return Syntax.code("({0} != {1})", a, b);
	}

	@:op(A < B) static inline function ltPre(a:EitherType<Int, BigInt>, b:BigInt):Bool {
		return Syntax.code("({0} < {1})", a, b);
	}

	@:op(A < B) static inline function ltPost(a:BigInt, b:EitherType<Int, BigInt>):Bool {
		return Syntax.code("({0} < {1})", a, b);
	}

	@:op(A <= B) static inline function lePre(a:EitherType<Int, BigInt>, b:BigInt):Bool {
		return Syntax.code("({0} <= {1})", a, b);
	}

	@:op(A <= B) static inline function lePost(a:BigInt, b:EitherType<Int, BigInt>):Bool {
		return Syntax.code("({0} <= {1})", a, b);
	}

	@:op(A > B) static inline function gtPre(a:EitherType<Int, BigInt>, b:BigInt):Bool {
		return Syntax.code("({0} > {1})", a, b);
	}

	@:op(A > B) static inline function gtPost(a:BigInt, b:EitherType<Int, BigInt>):Bool {
		return Syntax.code("({0} > {1})", a, b);
	}

	@:op(A >= B) static inline function gePre(a:EitherType<Int, BigInt>, b:BigInt):Bool {
		return Syntax.code("({0} >= {1})", a, b);
	}

	@:op(A >= B) static inline function gePost(a:BigInt, b:EitherType<Int, BigInt>):Bool {
		return Syntax.code("({0} >= {1})", a, b);
	}

	@:op(A + B) static inline function joinPre(a:String, b:BigInt):String {
		return Syntax.code("({0} + {1})", a, b);
	}

	@:op(A + B) static inline function joinPost(a:BigInt, b:String):String {
		return Syntax.code("({0} + {1})", a, b);
	}
}

@:native("BigInt")
private extern class _BigInt {
	@:selfCall @:pure function new(value:EitherType<String, Int>);

	@:pure function toLocaleString(?locales:EitherType<String, Array<String>>, ?options:NumberFormatOptions):String;

	@:pure function toString(?radix:Int):String;

	@:pure function valueOf():_BigInt;

	@:pure static function asIntN(width:Int, bigint:_BigInt):_BigInt;

	@:pure static function asUintN(width:Int, bigint:_BigInt):_BigInt;
}
