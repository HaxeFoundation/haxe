/*
 * Copyright (C)2014-2020 Haxe Foundation
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

package js.node;

#if haxe4
import js.lib.Error;
import js.lib.Promise;
import js.lib.RegExp;
#else
import js.Error;
import js.Promise;
import js.RegExp;
#end

/**
	The `assert module` provides a set of assertion functions for verifying invariants.
	The module provides a recommended [strict mode](https://nodejs.org/api/assert.html#assert_strict_mode) and a more lenient legacy mode.

	@see https://nodejs.org/api/assert.html#assert_assert
**/
@:jsRequire("assert")
extern class Assert {
	/**
		In strict mode, assert functions use the comparison in the corresponding strict functions.
		For example, `Assert.deepEqual()` will behave like `Assert.deepStrictEqual()`.

		@see https://nodejs.org/api/assert.html#assert_strict_mode
	**/
	static var strict(default, never):Assert;

	/**
		An alias of `Assert.ok()`.

		@see https://nodejs.org/api/assert.html#assert_assert_value_message
	**/
	@:selfCall
	@:overload(function(value:Dynamic, ?message:Error):Void {})
	static function assert(value:Dynamic, ?message:String):Void;

	/**
		An alias of `Assert.deepStrictEqual()`.

		@see https://nodejs.org/api/assert.html#assert_assert_deepequal_actual_expected_message
	**/
	@:deprecated
	@:overload(function<T>(actual:T, expected:T, ?message:Error):Void {})
	static function deepEqual<T>(actual:T, expected:T, ?message:String):Void;

	/**
		Tests for deep equality between the `actual` and `expected` parameters.
		"Deep" equality means that the enumerable "own" properties of child objects
		are recursively evaluated also by the following rules.

		@see https://nodejs.org/api/assert.html#assert_assert_deepstrictequal_actual_expected_message
	**/
	@:overload(function<T>(actual:T, expected:T, ?message:Error):Void {})
	static function deepStrictEqual<T>(actual:T, expected:T, ?message:String):Void;

	/**
		Awaits the `asyncFn` promise or, if `asyncFn` is a function,
		immediately calls the function and awaits the returned promise to complete.
		It will then check that the promise is not rejected.

		@see https://nodejs.org/api/assert.html#assert_assert_doesnotreject_asyncfn_error_message
	**/
	@:overload(function(asyncFn:Void->Promise<Dynamic>, ?error:Class<Dynamic>, ?message:String):Void {})
	@:overload(function(asyncFn:Void->Promise<Dynamic>, ?error:RegExp, ?message:String):Void {})
	@:overload(function(asyncFn:Void->Promise<Dynamic>, ?error:Dynamic->Bool, ?message:String):Void {})
	@:overload(function(asyncFn:Promise<Dynamic>, ?error:Class<Dynamic>, ?message:String):Void {})
	@:overload(function(asyncFn:Promise<Dynamic>, ?error:RegExp, ?message:String):Void {})
	static function doesNotReject(asyncFn:Promise<Dynamic>, ?error:Dynamic->Bool, ?message:String):Void;

	/**
		Asserts that the function `fn` does not throw an error.

		Using `Assert.doesNotThrow()` is actually not useful because there is no benefit
		in catching an error and then rethrowing it.
		Instead, consider adding a comment next to the specific code path that should not throw
		and keep error messages as expressive as possible.

		@see https://nodejs.org/api/assert.html#assert_assert_doesnotthrow_fn_error_message
	**/
	@:overload(function(fn:Void->Void, ?error:Class<Dynamic>, ?message:String):Void {})
	@:overload(function(fn:Void->Void, ?error:RegExp, ?message:String):Void {})
	static function doesNotThrow(fn:Void->Void, ?error:Dynamic->Bool, ?message:String):Void;

	/**
		An alias of `strictEqual`.

		@see https://nodejs.org/api/assert.html#assert_assert_equal_actual_expected_message
	**/
	@:overload(function<T>(actual:T, expected:T, ?message:String):Void {})
	static function equal<T>(actual:T, expected:T, ?message:Error):Void;

	/**
		Throws an `AssertionError` with the provided error message or a default error message.
		If the `message` parameter is an instance of an `Error` then it will be thrown instead of the `AssertionError`.

		@see https://nodejs.org/api/assert.html#assert_assert_fail_message
	**/
	@:overload(function(?message:String):Void {})
	static function fail(?message:Error):Void;

	/**
		Throws an `AssertionError`. If `message` is falsy, the error message is set as the values
		of `actual` and `expected` separated by the provided `operator`.
		Otherwise, the error message is the value of `message`.

		@see https://nodejs.org/api/assert.html#assert_assert_fail_actual_expected_message_operator_stackstartfn
	**/
	@:deprecated
	@:native("fail")
	@:overload(function<T>(actual:T, expected:T, ?message:String, ?operator_:String, ?stackStartFn:haxe.Constraints.Function):Void {})
	static function fail_<T>(actual:T, expected:T, ?message:Error, ?operator_:String, ?stackStartFn:haxe.Constraints.Function):Void;

	/**
		Throws `value` if `value` is not `undefined` or `null`.
		This is useful when testing the `error` argument in callbacks.
		The stack trace contains all frames from the error passed to `ifError()` including the potential new frames for `ifError()` itself.

		@see https://nodejs.org/api/assert.html#assert_assert_iferror_value
	**/
	static function ifError(value:Dynamic):Void;

	/**
		An alias of `Assert.notDeepStrictEqual()`.

		@see https://nodejs.org/api/assert.html#assert_assert_notdeepequal_actual_expected_message
	**/
	@:deprecated
	@:overload(function<T>(actual:T, expected:T, ?message:Error):Void {})
	static function notDeepEqual<T>(actual:T, expected:T, ?message:String):Void;

	/**
		Tests for deep strict inequality. Opposite of `Assert.deepStrictEqual()`.

		@see https://nodejs.org/api/assert.html#assert_assert_notdeepstrictequal_actual_expected_message
	**/
	@:overload(function<T>(actual:T, expected:T, ?message:Error):Void {})
	static function notDeepStrictEqual<T>(actual:T, expected:T, ?message:String):Void;

	/**
		An alias of `Assert.notStrictEqual()`.

		@see https://nodejs.org/api/assert.html#assert_assert_notequal_actual_expected_message
	**/
	@:deprecated
	@:overload(function<T>(actual:T, expected:T, ?message:Error):Void {})
	static function notEqual<T>(actual:T, expected:T, ?message:String):Void;

	/**
		Tests strict inequality between the `actual` and `expected` parameters as determined by the SameValue Comparison.

		@see https://nodejs.org/api/assert.html#assert_assert_notstrictequal_actual_expected_message
	**/
	@:overload(function<T>(actual:T, expected:T, ?message:Error):Void {})
	static function notStrictEqual<T>(actual:T, expected:T, ?message:String):Void;

	/**
		Tests if `value` is truthy.
		It is equivalent to `Assert.equal(!!value, true, message)`.

		@see https://nodejs.org/api/assert.html#assert_assert_ok_value_message
	**/
	@:overload(function(value:Dynamic, ?message:Error):Void {})
	static function ok(value:Dynamic, ?message:String):Void;

	/**
		Awaits the `asyncFn` promise or, if `asyncFn` is a function,
		immediately calls the function and awaits the returned promise to complete.
		It will then check that the promise is rejected.

		@see https://nodejs.org/api/assert.html#assert_assert_rejects_asyncfn_error_message
	**/
	@:overload(function(asyncFn:Void->Promise<Dynamic>, ?error:Class<Dynamic>, ?message:String):Void {})
	@:overload(function(asyncFn:Void->Promise<Dynamic>, ?error:RegExp, ?message:String):Void {})
	@:overload(function(asyncFn:Void->Promise<Dynamic>, ?error:Dynamic->Bool, ?message:String):Void {})
	@:overload(function(asyncFn:Void->Promise<Dynamic>, ?error:Dynamic, ?message:String):Void {})
	@:overload(function(asyncFn:Void->Promise<Dynamic>, ?error:Error, ?message:String):Void {})
	@:overload(function(asyncFn:Promise<Dynamic>, ?error:Class<Dynamic>, ?message:String):Void {})
	@:overload(function(asyncFn:Promise<Dynamic>, ?error:RegExp, ?message:String):Void {})
	@:overload(function(asyncFn:Promise<Dynamic>, ?error:Dynamic->Bool, ?message:String):Void {})
	@:overload(function(asyncFn:Promise<Dynamic>, ?error:Dynamic, ?message:String):Void {})
	static function rejects(asyncFn:Promise<Dynamic>, ?error:Error, ?message:String):Void;

	/**
		Tests strict equality between the `actual` and `expected` parameter as
		determined by the SameValue Comparison.

		@see https://nodejs.org/api/assert.html#assert_assert_strictequal_actual_expected_message
	**/
	@:overload(function<T>(actual:T, expected:T, ?message:Error):Void {})
	static function strictEqual<T>(actual:T, expected:T, ?message:String):Void;

	/**
		Expects the function `fn` to throw an error.

		@see https://nodejs.org/api/assert.html#assert_assert_throws_fn_error_message
	**/
	@:overload(function(fn:Void->Void, ?error:RegExp, ?message:String):Void {})
	@:overload(function(fn:Void->Void, ?error:Dynamic->Bool, ?message:String):Void {})
	@:overload(function(fn:Void->Void, ?error:Dynamic, ?message:String):Void {})
	static function throws(fn:Void->Void, ?error:Error, ?message:String):Void;
}
