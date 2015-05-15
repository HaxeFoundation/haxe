package unit;

import python.KwArgs;
import python.Syntax;
import python.VarArgs;
import sys.io.File;
import sys.io.Process;

// check compilation python classes
import python.NativeArrayTools;
import python.NativeStringTools;

import python.lib.Codecs;
import python.lib.Functools;
import python.lib.Glob;
import python.lib.Inspect;
import python.lib.Json;

import python.lib.Math;
import python.lib.Msvcrt;
import python.lib.Os;
import python.lib.Pprint;
import python.lib.Random;
import python.lib.Re;
import python.lib.Shutil;
import python.lib.Subprocess;
import python.lib.Sys;
import python.lib.Tempfile;
import python.lib.Termios;
import python.lib.ThreadLowLevel;
import python.lib.Time;
import python.lib.Traceback;
import python.lib.Tty;
import python.Tuple;
import python.Set;

import python.lib.datetime.Datetime;
import python.lib.datetime.Timedelta;
import python.lib.datetime.Timezone;
import python.lib.datetime.Tzinfo;

import python.lib.io.BufferedIOBase;
import python.lib.io.BufferedRWPair;
import python.lib.io.BufferedRandom;
import python.lib.io.BufferedReader;
import python.lib.io.BufferedWriter;
import python.lib.io.BytesIO;
import python.lib.io.FileIO;
import python.lib.io.IOBase;
import python.lib.io.RawIOBase;
import python.lib.io.StringIO;
import python.lib.io.TextIOBase;

import python.lib.net.Address;
import python.lib.net.Socket;

import python.lib.subprocess.Popen;

import python.lib.threading.Thread;

import python.lib.xml.etree.ElementTree;

import python.lib.json.JSONEncoder;




private typedef T = {
	var value:Int;
	@:optional var maybeValue:Int;
}

private enum MyEnum {
	A(?x:Int, b:String);
	True;
	False;
}

private interface IA {}

private class A implements IA { }

private class B extends A {
    public function new() {}
}

class TestPython extends Test {

	public function testDoWhileAsExpression () {
		var x = 1;
		var z = function () return (do {
			x++;
		} while (x < 3));

		z();

		eq(3, x);
	}

	public function testKeywords () {
		var list = new Array();
	}

	public function testStringMethod() {
		var d:Dynamic = "foo";
		eq("FOO", d.toUpperCase());

		var o:{function toUpperCase():String;} = "foo";
		eq("FOO", o.toUpperCase());

		var d:Dynamic = "FOO";
		eq("foo", d.toLowerCase());

		var o:{function toLowerCase():String;} = "FOO";
		eq("foo", o.toLowerCase());
	}


	public function testOptionalStructureFields() {
		var v:T = haxe.Json.parse('{"value": 1 }');
		eq(1, v.value);
		eq(null, v.maybeValue);
		v.maybeValue = 12;
		eq(12, v.maybeValue);
		v.maybeValue += 9;
		eq(21, v.maybeValue);

		var v:T = haxe.Json.parse('{"value": 1 }');
		var d:Dynamic = v;
		eq(1, d.value);
		eq(null, d.maybeValue);
		d.maybeValue = 12;
		eq(12, d.maybeValue);
		d.maybeValue += 9;
		eq(21, d.maybeValue);
	}

	function testNonOptionalArgumentAfterOptionalArgument() {
		function skip(a:Int = 1, b:String) {
			return Std.string(a) + b;
		}
		eq("12a", skip(12, "a"));
		eq("1a", skip("a"));
	}

	function testNativeClosures() {
        var s = "foo";
        var f = s.toUpperCase;
		eq("FOO", f());

		var a = [];
		var f = a.push;
		f(12);
		eq(12, a[0]);
	}

	function testOptionalEnumArguments() {
		var a1 = 1;
		var a2 = null;
		switch(A("foo")) {
			case A(i, b):
				a1 = i;
				a2 = b;
			case _:
		}
		eq(null, a1);
		eq("foo", a2);
	}

	function testTypedCatch() {
		function throwMe(arg:Dynamic) {
			return try {
				throw arg;
			} catch(e:haxe.macro.Expr.ExprDef) {
				'ExprDef:$e';
			} catch(s:String) {
				'String:$s';
			} catch(e:Dynamic) {
				'Other:$e';
			}
		}
		eq("ExprDef:EBreak", throwMe(haxe.macro.Expr.ExprDef.EBreak));
		eq("String:foo", throwMe("foo"));
		eq("Other:12", throwMe(12));
	}

	/*
	function testSys () {

		var p = new Process("/bin/ls", ["-l"]);

		trace(p.stdout.readLine());
		trace(p.stdout.readLine());
	}
	*/

	function testUnderscoreAndReflection () {
		var x = { __v : 5 };
		eq(5, Reflect.field(x, "__v"));

		var x = { ___b : 5 };
		eq(5, Reflect.field(x, "___b"));

		var x = { __iter__ : 5 };
		eq(5, Reflect.field(x, "__iter__"));
	}


	function testMakeVarArgs () {
		var f = function (a:Array<Dynamic>) {
			return a[0] + a[1];
		}
		var g = Reflect.makeVarArgs(f);
		var res = g(1,2);
		eq(3, res);
	}

	function testKwArgsAfterVarArgs () {
		function test (va:VarArgs<Dynamic>, kw:KwArgs<Dynamic>) {
			var a = va.toArray();

			eq(1,a[0]);
			eq(2,a[1]);
			eq(1,kw.get("a", null));
		}
		var a = python.Lib.anonToDict({ "a" : 1});
		var x = [1,2];
		test(x,a);
	}

	function testSoftKeywords () {
		function test (len:String, bytes:String) {
			eq(len.length,bytes.length);
		}
		test("x", "x");
	}

	function testKwArgsNativeNames () {
		function test (?kw:KwArgs<{ @:native("default") var def:Int; }>) {
			eq(1, kw.typed().def);
		}

		test({ def : 1});
	}

	function testOptionalVarArgs () {
		function test (?va:VarArgs<Dynamic>, ?kw:KwArgs<Dynamic>) {
			var a = va.toArray();

			eq(0,a.length);
		}
		test();
	}

	function testOptionalKwArgs () {
		function test (?kw:KwArgs<Dynamic>) eq(0,kw.toDict().length);
		test();
	}

	function testOptionalKwArgsAfterOptionalVarArgs () {
		function test (?va:VarArgs<Dynamic>, ?kw:KwArgs<Dynamic>) {
			var a = va.toArray();

			eq(1,a[0]);
			eq(2,a[1]);

			eq(0, kw.toDict().length);
		}
		var x = [1,2];
		test(x);

		function test (?va:VarArgs<Dynamic>, ?kw:KwArgs<Dynamic>) {
			var a = va.toArray();
			eq(0,a.length);
			eq(1, kw.get("a",null));
		}

		var a = python.Lib.anonToDict({ "a" : 1});

		test(a);
	}

	function testKwArgs () {
		function x (args:KwArgs<Dynamic>) {
			var a = args.get("a", 0);
			var b = args.get("b", 0);
			return a + b;
		}

		var a = python.Lib.anonToDict({ "a" : 1, "b" : 2});
		var res = x( a );

		eq(3, res);

		var res2 = python.Syntax.callNamedUntyped(x, { a : 3, b : 5});

		eq(8, res2);
	}

	function testTypedKwArgs () {
		function x (args:KwArgs<{ a : Int, b : Int}>) {
			var x = args.typed();

			return x.a + x.b;
		}

		var a = { a : 1, b : 2};
		var res = x( a );

		eq(3, res);

		var res = x( { a : 1, b : 2} );

		eq(3, res);
	}

	function testNonLocal() {
		try { }
		catch (e:Dynamic) {
			e = 1;
		}
	}

	var _s:String;

	var s(get, null):String;
	var s2(null, set_s2):String;
	var s3(get, set):String;

	function get_s() return s;
	function set_s2(s) return s2 = s;
	function get_s3() return _s;
	function set_s3(s) return _s = s;

    function testPropertyInit() {
		s += "a";
		s2 += "b";
		s3 += "c";
		eq("nulla", s);
		eq("nullb", s2);
		eq("nullc", s3);
    }

	function testIsViaParentInterface() {
		t((new B() is IA));
	}


	// Syntax Tests

	function testPythonCodeStringInterpolation () {
		var z = 1;
		var a = (Syntax.pythonCode('[{0}, {1}]', z, 2):Array<Int>);

		eq(a[0], z);
		eq(a[1], 2);

		function test2 (x:Int) {
			x += 1;
			return (Syntax.pythonCode("{0}", x):Int);
		}

		function test3 (x:Int) {
			return (Syntax.pythonCode('[{0}]', x):Array<Int>);
		}

		var x = 1;

		eq(2, test2(x));
		eq(1, x);
		eq(1, test3(1)[0]);

		eq("foo1bar", Syntax.pythonCode("'foo' + str({0}) + 'bar'", x));


		function test4a (x:Int) {
			return (Syntax.pythonCode("[{0}][0]", x+x):Int);
		}

		function test4b (x:Int):String {
			return Syntax.pythonCode('[{0}][0]', (function () return Std.string(x+x))() );
		}

		eq(2, test4a(1));
		eq("2", test4b(1));
	}

	function testTupleCreation() {
		var t = Tuple1.make(1);
		eq(t._1, 1);
		eq(t.length, 1);

		var t = Tuple2.make(1, 2);
		eq(t._1, 1);
		eq(t._2, 2);
		eq(t.length, 2);

		var t = Tuple3.make(1, 2, 3);
		eq(t._1, 1);
		eq(t._2, 2);
		eq(t._3, 3);
		eq(t.length, 3);

		var t = Tuple4.make(1, 2, 3, 4);
		eq(t._1, 1);
		eq(t._2, 2);
		eq(t._3, 3);
		eq(t._4, 4);
		eq(t.length, 4);

		var t = Tuple5.make(1, 2, 3, 4, 5);
		eq(t._1, 1);
		eq(t._2, 2);
		eq(t._3, 3);
		eq(t._4, 4);
		eq(t._5, 5);
		eq(t.length, 5);

		var t = new Tuple([1,2,3]);
		eq(t[0], 1);
		eq(t[1], 2);
		eq(t[2], 3);
		eq(t.length, 3);
	}

}