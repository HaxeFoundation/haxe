package unit;

import python.KwArgs;
import python.NativeArrayTools;
import python.NativeStringTools;
import python.Set;
import python.Syntax;
import python.Tuple;
import python.VarArgs;
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
import python.lib.json.JSONEncoder;
import python.lib.socket.Address;
import python.lib.socket.Socket;
import python.lib.subprocess.Popen;
import python.lib.threading.Thread;
import python.lib.xml.etree.ElementTree;
import sys.io.File;
import sys.io.Process;

// check compilation python classes

private typedef T = {
	var value:Int;
	var ?maybeValue:Int;
}

private interface IA {}
private class A implements IA {}

private class B extends A {
	public function new() {}
}

private enum MyEnum {
	A(?x:Int, b:String);
	True;
	False;
}

// A module made by a file included before the imports (testIncludeFileTop).
@:pythonImport("haxe_include_file_top")
private extern class IncludedModule {
	static var value(default, never):Int;

	static function __init__():Void {
		haxe.macro.Compiler.includeFile("unit/issues/misc/PythonIncludeFileTop.py");
	}
}

class TestPython extends Test {
	function testIncludeFileTop() {
		eq(42, IncludedModule.value);
	}

	public function testDoWhileAsExpression() {
		var x = 1;
		var z = function() return (do {
			x++;
		} while (x < 3));

		z();

		eq(3, x);
	}

	public function testKeywords() {
		var list = new Array();
		noAssert();
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
		switch (A("foo")) {
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
			} catch (e:haxe.macro.Expr.ExprDef) {
				'ExprDef:$e';
			} catch (s:String) {
				'String:$s';
			} catch (e:Dynamic) {
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
	function testUnderscoreAndReflection() {
		var x = {__v: 5};
		eq(5, Reflect.field(x, "__v"));

		var x = {___b: 5};
		eq(5, Reflect.field(x, "___b"));

		var x = {__iter__: 5};
		eq(5, Reflect.field(x, "__iter__"));
	}

	function testKwArgsAfterVarArgs() {
		function test(va:VarArgs<Dynamic>, kw:KwArgs<Dynamic>) {
			var a = va.toArray();

			eq(1, a[0]);
			eq(2, a[1]);
			eq(1, kw.get("a", null));
		}
		var a = python.Lib.anonToDict({"a": 1});
		var x = [1, 2];
		test(x, a);
	}

	function testSoftKeywords() {
		function test(len:String, bytes:String) {
			eq(len.length, bytes.length);
		}
		test("x", "x");
	}

	function testAnonDictFieldNames() {
		// Python stores fields named like keywords or with a leading "__" under other names; the
		// conversions use the field names.
		var o:Dynamic = {"from": 1, "pass": 2, "__private": 3, normal: 4};
		Reflect.hasField(o, "normal"); // sets _hx_disable_getattr on the object, which isn't a field
		var d = python.Lib.anonToDict(o);
		eq(1, d.get("from", null));
		eq(2, d.get("pass", null));
		eq(3, d.get("__private", null));
		eq(4, d.get("normal", null));
		eq(4, d.length);
		var back:Dynamic = python.Lib.dictToAnon(d);
		eq(1, back.from);
		eq(2, back.pass);
		eq(3, Reflect.field(back, "__private"));
		var n:Dynamic = {"nonlocal": 5};
		eq(5, n.nonlocal);
	}

	static function isDict(v:Dynamic):Bool
		return Syntax.code("isinstance({0}, dict)", v);

	function testAnonDictDeep() {
		var o:Dynamic = {a: {"from": [1, 2]}, list: ([{b: 1}, 2, [{c: 3}]] : Array<Dynamic>)};
		var d = python.Lib.anonToDict(o, true);
		var a:python.Dict<String, Dynamic> = d.get("a", null);
		t(isDict(a));
		eq(2, (a.get("from", null) : Array<Int>)[1]);
		var list:Array<Dynamic> = d.get("list", null);
		t(isDict(list[0]));
		t(isDict(list[2][0]));
		// a flat conversion keeps the nested objects
		f(isDict(python.Lib.anonToDict(o).get("a", null)));

		var back:Dynamic = python.Lib.dictToAnon(d, true);
		eq(2, back.a.from[1]);
		eq(1, back.list[0].b);
		eq(3, back.list[2][0].c);
		eq('{"a":{"from":[1,2]},"list":[{"b":1},2,[{"c":3}]]}', haxe.Json.stringify(back));

		// a dictionary with other than string keys stays a dictionary
		var numbers:python.Dict<Dynamic, Dynamic> = Syntax.code("{1: 'one'}");
		var outer = new python.Dict<String, Dynamic>();
		outer.set("numbers", numbers);
		var o2:Dynamic = python.Lib.dictToAnon(outer, true);
		t(isDict(o2.numbers));
		eq("one", (o2.numbers : python.Dict<Int, String>).get(1, null));
	}

	// haxe.crypto.Base64 uses Python's base64 module here; it decodes as the generic version does.
	function testBase64LikeGeneric() {
		exc(() -> haxe.crypto.Base64.decode("YQ==", false)); // "=" isn't expected without complement
		exc(() -> haxe.crypto.Base64.decode("Y Q=="));
		exc(() -> haxe.crypto.Base64.urlDecode("YWI/")); // the other alphabet
		exc(() -> haxe.crypto.Base64.decode("YWI_"));
		eq("ab~", haxe.crypto.Base64.decode("YWJ+Y").toString()); // a char left over is ignored
		eq("a", haxe.crypto.Base64.decode("YQ===").toString());
		eq("", haxe.crypto.Base64.decode("").toString());
	}

	function testKwArgsKeywordNames() {
		var fn:KwArgs<{from:Int, normal:Int}>->python.Dict<String, Dynamic> = Syntax.code("(lambda **kw: kw)");
		var kw = fn({from: 1, normal: 2});
		eq(1, kw.get("from", null));
		eq(2, kw.get("normal", null));
		function typed(?kw:KwArgs<{from:Int}>)
			return kw.typed().from;
		eq(3, typed({from: 3}));
	}

	function testKwArgsNativeNames() {
		function test(?kw:KwArgs<{@:native("default") var def:Int;}>) {
			eq(1, kw.typed().def);
		}

		test({def: 1});
	}

	function testKwArgsQuotedNames() {
		// Python receives the quoted names, also Python keywords ("class")
		var fn:KwArgs<{var "default":Int; var "class":String;}>->python.Dict<String, Dynamic> = Syntax.code("(lambda **kw: kw)");
		var kw = fn({"default": 1, "class": "x"});
		eq(1, kw.get("default", null));
		eq("x", kw.get("class", null));
		eq(2, kw.length);

		function test(?kw:KwArgs<{var "default":Int;}>)
			return kw.typed()."default";
		eq(3, test({"default": 3}));
	}

	function testOptionalVarArgs() {
		function test(?va:VarArgs<Dynamic>, ?kw:KwArgs<Dynamic>) {
			var a = va.toArray();

			eq(0, a.length);
		}
		test();
	}

	function testOptionalKwArgs() {
		function test(?kw:KwArgs<Dynamic>)
			eq(0, kw.toDict().length);
		test();
	}

	function testOptionalKwArgsAfterOptionalVarArgs() {
		function test(?va:VarArgs<Dynamic>, ?kw:KwArgs<Dynamic>) {
			var a = va.toArray();

			eq(1, a[0]);
			eq(2, a[1]);

			eq(0, kw.toDict().length);
		}
		var x = [1, 2];
		test(x);

		function test(?va:VarArgs<Dynamic>, ?kw:KwArgs<Dynamic>) {
			var a = va.toArray();
			eq(0, a.length);
			eq(1, kw.get("a", null));
		}

		var a = python.Lib.anonToDict({"a": 1});

		test(a);
	}

	function testKwArgs() {
		function x(args:KwArgs<Dynamic>) {
			var a = args.get("a", 0);
			var b = args.get("b", 0);
			return a + b;
		}

		var a = python.Lib.anonToDict({"a": 1, "b": 2});
		var res = x(a);

		eq(3, res);

		var res2 = python.Syntax.callNamedUntyped(x, {a: 3, b: 5});

		eq(8, res2);
	}

	function testTypedKwArgs() {
		function x(args:KwArgs<{a:Int, b:Int}>) {
			var x = args.typed();

			return x.a + x.b;
		}

		var a = {a: 1, b: 2};
		var res = x(a);

		eq(3, res);

		var res = x({a: 1, b: 2});

		eq(3, res);
	}

	function testNonLocal() {
		try {} catch (e:Dynamic) {
			e = 1;
		}
		noAssert();
	}

	var _s:String;

	var s(get, null):String;
	var s2(null, set):String;
	var s3(get, set):String;

	function get_s()
		return s;

	function set_s2(s)
		return s2 = s;

	function get_s3()
		return _s;

	function set_s3(s)
		return _s = s;

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

	function testPythonCodeStringInterpolation() {
		var z = 1;
		var a = (Syntax.code('[{0}, {1}]', z, 2) : Array<Int>);

		eq(a[0], z);
		eq(a[1], 2);

		function test2(x:Int) {
			x += 1;
			return (Syntax.code("{0}", x) : Int);
		}

		function test3(x:Int) {
			return (Syntax.code('[{0}]', x) : Array<Int>);
		}

		var x = 1;

		eq(2, test2(x));
		eq(1, x);
		eq(1, test3(1)[0]);

		eq("foo1bar", Syntax.code("'foo' + str({0}) + 'bar'", x));

		function test4a(x:Int) {
			return (Syntax.code("[{0}][0]", x + x) : Int);
		}

		function test4b(x:Int):String {
			return Syntax.code('[{0}][0]', (function() return Std.string(x + x))());
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

		var t = new Tuple([1, 2, 3]);
		eq(t[0], 1);
		eq(t[1], 2);
		eq(t[2], 3);
		eq(t.length, 3);
	}

	function testVectorEquality() {
		var v = new haxe.ds.Vector(1);
		var v2 = v.copy();
		eq(v == v2, false);
	}
}
