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
import Reflect;

class TestRunner {
	var result : TestResult;
	var cases  : List<TestCase>;

	private static function print( v : Dynamic ){
		#if flash
		untyped {
						var root = flash.Lib.current;
						var tf = root.__trace_txt;
						if( tf == null ) {
										root.createTextField("__trace_txt",1048500,0,0,Stage.width,Stage.height);
										tf = root.__trace_txt;
										tf.selectable = false;
										tf.wordWrap = true;
										root.__trace_lines = new Array<String>();
						}
						var s = flash.Boot.__string_rec(v,"");
						tf.text += s;
						var lines = tf.text.split("\n");
						var nlines = Stage.height / 16;
						if( lines.length > nlines ){
										lines.splice(0,lines.length-nlines);
							tf.text = lines.join("\n");
						}
		}
		#else neko
		untyped __dollar__print(v);
		#else js
		untyped {
			var msg = StringTools.htmlEscape(js.Boot.__string_rec(v,"")).split("\n").join("<br/>");
			var d = document.getElementById("haxe:trace");
			if( d == null )
				alert("haxe:trace element not found")
			else
				d.innerHTML += msg;
		}
		#else error
		#end
	}

	public function new() {
		result = new TestResult();
		cases = new List();
	}

	public function add( c:TestCase ) : Void{
		cases.add(c);
	}

	public function run() : Void {
		result = new TestResult();
		for ( c in cases ){
			runCase(c);
		}
		print(result.toString());
	}

	function runCase( t:TestCase ) : Void 	{
		var fields = Reflect.fields( Reflect.getClass(t).prototype );

		print( "Class: "+Reflect.getClass(t).__name__[0]+" ");
		for ( f in fields ){
			var fname = f;
			var field = Reflect.field(t, f);
			if (StringTools.startsWith(fname,__unprotect__("test")) && Reflect.isFunction(field) ){
				t.currentTest = new TestStatus();
				t.currentTest.classname = Reflect.getClass(t).__name__.join(".");
				t.currentTest.method = fname;
				t.setup();

				try {
					Reflect.callMethod(t, field, new Array());

					if( t.currentTest.done ){
						t.currentTest.success = true;
						print(".");
					}else{
						t.currentTest.success = false;
						t.currentTest.error = "(warning) no assert";
						print("W");
					}
				}catch ( e : TestStatus ){
					print("F");
					#if neko
					t.currentTest.backtrace = neko.Stack.exceptionStack();
					#end
				}catch ( e : Dynamic ){
					print("E");
					#if js
					if( e.message != null ){
						t.currentTest.error = "exception thrown : "+e+" ["+e.message+"]";
					}else{
						t.currentTest.error = "exception thrown : "+e;
					}
					#else true
					t.currentTest.error = "exception thrown : "+e;
					#end
					#if neko
					t.currentTest.backtrace = neko.Stack.exceptionStack();
					#else js
					if( Reflect.typeof(e) == TObject ){
						if( e.stack != null ){
							t.currentTest.backtrace = e.stack;
						}
					}
					#end
				}
				result.add(t.currentTest);
				t.tearDown();
			}
		}

		print("\n");
	}
}
