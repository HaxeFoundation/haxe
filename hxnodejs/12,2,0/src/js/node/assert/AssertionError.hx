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

package js.node.assert;

#if haxe4
import js.lib.Error;
#else
import js.Error;
#end

/**
	Indicates the failure of an assertion. All errors thrown by the `Assert` module will be instances of the `AssertionError` class.

	@see https://nodejs.org/api/assert.html#assert_class_assert_assertionerror
**/
@:jsRequire("assert", "AssertionError")
extern class AssertionError extends Error {
	/**
		A subclass of Error that indicates the failure of an assertion.
	**/
	function new(options:AssertionErrorOptions);

	/**
		Set to the `actual` argument for methods such as `Assert.strictEqual()`.
	**/
	var actual:Dynamic;

	/**
		Set to the `expected` value for methods such as `Assert.strictEqual()`.
	**/
	var expected:Dynamic;

	/**
		Indicates if the message was auto-generated (`true`) or not.
	**/
	var generatedMessage:Bool;

	/**
		Value is always `ERR_ASSERTION` to show that the error is an assertion error.
	**/
	var code:String;

	/**
		Set to the passed in operator value.
	**/
	@:native("operator") var operator_:String;
}

/**
	An options type for `new` of `AssertionError`.
**/
typedef AssertionErrorOptions = {
	/**
		If provided, the error message is set to this value.
	**/
	@:optional var message:String;

	/**
		The `actual` property on the error instance.
	**/
	@:optional var actual:Dynamic;

	/**
		The `expected` property on the error instance.
	**/
	@:optional var expected:Dynamic;

	#if (haxe_ver < 4)
	/**
		The `operator` property on the error instance.
	**/
	@:optional var operator:String;
	#end

	/**
		If provided, the generated stack trace omits frames before this function.
	**/
	@:optional var stackStartFunction:Dynamic;
}
