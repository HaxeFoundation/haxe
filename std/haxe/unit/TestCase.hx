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
import haxe.PosInfos;

/**
	This unit test class should be extended to create test cases. Each test 
	method created in this extended class should start with the name "test".

	These test methods should call the assertion methods:

	 * `assertTrue(a)`: Succeeds if `a` is `true`.
	 * `assertFalse(a)`: Succeeds if `a` is `false`.
	 * `assertEquals(expected, actual)`: Succeeds if `expected` and `actual`
	   are equal.

	```haxe
	class MyTestCase extends haxe.unit.TestCase {
		function testBasic() {
			assertEquals("A", "A");
		}
	}
	```

	The TestCase can be tested using `TestRunner`. 

	To run code before or after the test, override the functions `setup` 
	and `tearDown`.

	@see <https://haxe.org/manual/std-unit-testing.html>
**/
@:keepSub
@:publicFields
class TestCase {
	/**
		The current test status of the TestRunner.
	**/
	public var currentTest : TestStatus;

	public function new( ) {
	}

	/**
		Override this method to execute code before the test runs.
	**/
	public function setup() : Void {
	}

	/**
		Override this method to execute code after the test ran.
	**/
	public function tearDown() : Void {
	}

	function print( v : Dynamic ) {
		haxe.unit.TestRunner.print(v);
	}

	/**
		Succeeds if `b` is `true`.
	**/
	function assertTrue( b:Bool, ?c : PosInfos ) : Void {
		currentTest.done = true;
		if (b != true){
			currentTest.success = false;
			currentTest.error   = "expected true but was false";
			currentTest.posInfos = c;
			throw currentTest;
		}
	}

	/**
		Succeeds if `b` is `false`.
	**/
	function assertFalse( b:Bool, ?c : PosInfos ) : Void {
		currentTest.done = true;
		if (b == true){
			currentTest.success = false;
			currentTest.error   = "expected false but was true";
			currentTest.posInfos = c;
			throw currentTest;
		}
	}

	/**
		Succeeds if `expected` and `actual` are equal.
	**/
	function assertEquals<T>( expected: T , actual: T,  ?c : PosInfos ) : Void {
		currentTest.done = true;
		if (actual != expected){
			currentTest.success = false;
			currentTest.error   = "expected '" + expected + "' but was '" + actual + "'";
			currentTest.posInfos = c;
			throw currentTest;
		}
	}
}
