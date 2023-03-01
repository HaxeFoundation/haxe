/*
 * Copyright (C)2005-2022 Haxe Foundation
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

/* Original code courtesy Chuck Batson (github.com/cbatson) */
enum abstract BigIntExceptions(String) {
	var INVALID_ARGUMENT = "Invalid argument";
	var BUFFER_TOO_SMALL = "Buffer too small";
	var DIVISION_BY_ZERO = "Division by zero";
	var NEGATIVE_EXPONENT = "Negative exponent";
	var INVALID_OPERATION = "Invalid operation";
	var NEGATIVE_MODULUS = "Modulus should be positive";
	var EVEN_VALUES = "Both values are even";
}
