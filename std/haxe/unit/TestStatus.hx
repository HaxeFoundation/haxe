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
package haxe.unit;
import haxe.CallStack;

import haxe.PosInfos;

/**
	The status information of a unit test case method.

	@see <https://haxe.org/manual/std-unit-testing.html>
**/
class TestStatus {
	/**
		`true` when the unit test is executed.
	**/
	public var done : Bool;

	/**
		`true` when succesfully unit tested.
	**/
	public var success : Bool;

	/**
		The error message of the unit test method.
	**/
	public var error : String;

	/**
		The method name of the unit test.
	**/
	public var method : String;

	/**
		The class name of the unit test.
	**/
	public var classname : String;

	/**
		The position information of the unit test.
	**/
	public var posInfos : PosInfos;

	/**
		The representation of the stack exception.
	**/
	public var backtrace : String;

	public function new() 	{
		done = false;
		success = false;
	}
}
