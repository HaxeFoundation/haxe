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

package php;

/**
	Exception thrown if an error which can only be found on runtime occurs.
**/
@:native('RuntimeException')
extern class RuntimeException extends Exception {}

/**
	Exception thrown if a value is not a valid key.
	This represents errors that cannot be detected at compile time.
**/
@:native('OutOfBoundsException')
extern class OutOfBoundsException extends RuntimeException {}

/**
	Exception thrown when adding an element to a full container.
**/
@:native('OverflowException')
extern class OverflowException extends RuntimeException {}

/**
	Exception thrown to indicate range errors during program execution.
	Normally this means there was an arithmetic error other than under/overflow.
**/
@:native('RangeException')
extern class RangeException extends RuntimeException {}

/**
	Exception thrown when performing an invalid operation on an empty container,
	such as removing an element.
**/
@:native('UnderflowException')
extern class UnderflowException extends RuntimeException {}

/**
	Exception thrown if a value does not match with a set of values.
	Typically this happens when a function calls another function and
	expects the return value to be of a certain type or value not including
	arithmetic or buffer related errors.
**/
@:native('UnexpectedValueException')
extern class UnexpectedValueException extends RuntimeException {}
