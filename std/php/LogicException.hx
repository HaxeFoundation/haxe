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
	Exception that represents error in the program logic.
	This kind of exception should lead directly to a fix in your code.
**/
@:native('LogicException')
extern class LogicException extends Exception {}

/**
	Exception thrown if a callback refers to an undefined function
	or if some arguments are missing.
**/
@:native('BadFunctionCallException')
extern class BadFunctionCallException extends LogicException {}

/**
	Exception thrown if a callback refers to an undefined method
	or if some arguments are missing.
**/
@:native('BadMethodCallException')
extern class BadMethodCallException extends BadFunctionCallException {}

/**
	Exception thrown if a value does not adhere to a defined valid data domain.
**/
@:native('DomainException')
extern class DomainException extends LogicException {}

/**
	Exception thrown if an argument is not of the expected type.
**/
@:native('InvalidArgumentException')
extern class InvalidArgumentException extends LogicException {}

/**
	Exception thrown if a length is invalid.
**/
@:native('LengthException')
extern class LengthException extends LogicException {}

/**
	Exception thrown when an illegal index was requested.
	This represents errors that should be detected at compile time.
**/
@:native('OutOfRangeException')
extern class OutOfRangeException extends LogicException {}
