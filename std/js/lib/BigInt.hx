/*
 * Copyright (C)2005-2026 Haxe Foundation
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

import haxe.extern.EitherType;

/**
	`BigInt` values represent integers of arbitrary magnitude.

	Arithmetic operators are not available on `BigInt` values in Haxe; use `js.Syntax.code`
	(e.g. `js.Syntax.code("{0} + {1}", a, b)`) when needed.

	Documentation [BigInt](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("BigInt")
extern class BigInt {
	/**
		Creates a `BigInt` from an integer number, a string or a boolean.

		NOTE: Unlike in plain JavaScript, `new BigInt()` syntax is used in Haxe. This generates a `BigInt(...)`
		expression as required by the JavaScript specification.
	**/
	@:pure @:selfCall function new(value:EitherType<Float, EitherType<String, Bool>>);

	/**
		Clamps a `BigInt` value to the given number of bits, and returns that value as a signed integer.
	**/
	@:pure static function asIntN(bits:Int, bigint:BigInt):BigInt;

	/**
		Clamps a `BigInt` value to the given number of bits, and returns that value as an unsigned integer.
	**/
	@:pure static function asUintN(bits:Int, bigint:BigInt):BigInt;

	/**
		Returns a string representing this `BigInt` value in the specified radix (base).
	**/
	@:pure function toString(?radix:Int):String;

	/**
		Returns a string with a language-sensitive representation of this `BigInt` value.
	**/
	@:pure function toLocaleString(?locales:EitherType<String, Array<String>>, ?options:js.lib.intl.NumberFormat.NumberFormatOptions):String;

	/**
		Returns the wrapped primitive value of a `BigInt` object.
	**/
	@:pure function valueOf():BigInt;
}
