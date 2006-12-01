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

class TestResult {

	var m_tests : List<TestStatus>;
	public var success(default,null) : Bool;

	public function new() {
		m_tests = new List();
		success = true;
	}

	public function add( t:TestStatus ) : Void {
		m_tests.add(t);
		if( !t.success )
			success = false;
	}

	public function toString() : String 	{
		var buf = new StringBuf();
		var failures = 0;
		for ( test in m_tests ){
			if (test.success == false){
				buf.add("* ");
				buf.add(test.classname);
				buf.add("::");
				buf.add(test.method);
				buf.add("()");
				buf.add("\n");

				buf.add("ERR: ");
				if( test.posInfos != null ){
					buf.add(test.posInfos.fileName);
					buf.add(":");
					buf.add(test.posInfos.lineNumber);
					buf.add("(");
					buf.add(test.posInfos.className);
					buf.add(".");
					buf.add(test.posInfos.methodName);
					buf.add(") - ");
				}
				buf.add(test.error);
				buf.add("\n");

				if (test.backtrace != null){
					#if flash9
					buf.add(test.backtrace);
					#else true
					buf.add(haxe.Stack.toString(test.backtrace));
					#end
					buf.add("\n");
				}

				buf.add("\n");
				failures++;
			}
		}
		buf.add("\n");
		if (failures == 0)
			buf.add("OK ");
		else
			buf.add("FAILED ");

		buf.add(m_tests.length);
		buf.add(" tests, ");
		buf.add(failures);
		buf.add(" failed, ");
		buf.add( (m_tests.length - failures) );
		buf.add(" success");
		buf.add("\n");
		return buf.toString();
	}

}
