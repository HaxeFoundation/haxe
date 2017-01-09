/*
 * Copyright (C)2005-2017 Haxe Foundation
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

// This file is generated from mozilla\ValidityState.webidl. Do not edit!

package js.html;

/**
	The `ValidityState` interface represents the validity states that an element can be in, with respect to constraint validation. Together, they help explain why an element's value fails to validate, if it's not valid.

	Documentation [ValidityState](https://developer.mozilla.org/en-US/docs/Web/API/ValidityState) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/ValidityState$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/ValidityState>
**/
@:native("ValidityState")
extern class ValidityState
{
	
	/**
		Is a `Boolean` indicating the element has a `required` attribute, but no value.
	**/
	var valueMissing(default,null) : Bool;
	
	/**
		Is a `Boolean` indicating the value is not in the required syntax (when `type` is `email` or `url`).
	**/
	var typeMismatch(default,null) : Bool;
	
	/**
		Is a `Boolean` indicating the value does not match the specified `pattern`.
	**/
	var patternMismatch(default,null) : Bool;
	
	/**
		Is a `Boolean` indicating the value exceeds the specified `maxlength` for `HTMLInputElement` or `HTMLTextAreaElement` objects. Note: This will never be `true` in Gecko, because elements' values are prevented from being longer than `maxlength`.
	**/
	var tooLong(default,null) : Bool;
	
	/**
		Is a `Boolean` indicating the value is less than the minimum specified by the `min` attribute.
	**/
	var rangeUnderflow(default,null) : Bool;
	
	/**
		Is a `Boolean` indicating the value is greater than the maximum specified by the `max` attribute.
	**/
	var rangeOverflow(default,null) : Bool;
	
	/**
		Is a `Boolean` indicating the value does not fit the rules determined by the `step` attribute (that is, it's not evenly divisible by the step value).
	**/
	var stepMismatch(default,null) : Bool;
	
	/**
		Is a `Boolean` indicating the user has provided input that the browser is unable to convert.
	**/
	var badInput(default,null) : Bool;
	
	/**
		Is a `Boolean` indicating the element's custom validity message has been set to a non-empty string by calling the element's `setCustomValidity()` method.
	**/
	var customError(default,null) : Bool;
	
	/**
		Is a `Boolean` indicating the element meets all constraint validations, and is therefore considered to be valid.
	**/
	var valid(default,null) : Bool;
	
}