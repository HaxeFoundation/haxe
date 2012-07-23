/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package haxe.unit;
import haxe.PosInfos;

@:keepSub
class TestCase #if mt_build implements mt.Protect, #end implements haxe.Public  {
	public var currentTest : TestStatus;

	public function new( ) {
	}

	public function setup() : Void {
	}

	public function tearDown() : Void {
	}

	function print( v : Dynamic ) {
		haxe.unit.TestRunner.print(v);
	}

	function assertTrue( b:Bool, ?c : PosInfos ) : Void {
		currentTest.done = true;
		if (b == false){
			currentTest.success = false;
			currentTest.error   = "expected true but was false";
			currentTest.posInfos = c;
			throw currentTest;
		}
	}

	function assertFalse( b:Bool, ?c : PosInfos ) : Void {
		currentTest.done = true;
		if (b == true){
			currentTest.success = false;
			currentTest.error   = "expected false but was true";
			currentTest.posInfos = c;
			throw currentTest;
		}
	}

	function assertEquals<T>( expected: T , actual: T,  ?c : PosInfos ) : Void 	{
		currentTest.done = true;
		if (actual != expected){
			currentTest.success = false;
			currentTest.error   = "expected '" + expected + "' but was '" + actual + "'";
			currentTest.posInfos = c;
			throw currentTest;
		}
	}

}
