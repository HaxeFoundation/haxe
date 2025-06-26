/*
 * Copyright (C)2005-2023 Haxe Foundation
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

package haxe.math.bigint;

enum abstract BigIntError(String) to String {
	/** An argument provided to a function was not valid. */
	var INVALID_ARGUMENT = "Invalid argument";
	/** An output buffer was too small to hold the result. */
	var BUFFER_TOO_SMALL = "Buffer too small";
	/** An attempt was made to divide by zero. */
	var DIVISION_BY_ZERO = "Division by zero";
	/** An exponent was negative where not supported. */
	var NEGATIVE_EXPONENT = "Negative exponent";
	/** An operation was performed that is not valid. */
	var INVALID_OPERATION = "Invalid operation";
	/** A modulus was negative where it must be positive. */
	var NEGATIVE_MODULUS = "Modulus should be positive";
	/** An operation required at least one odd number, but both were even. */
	var EVEN_VALUES = "Both values are even";
}