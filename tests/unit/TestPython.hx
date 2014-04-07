package unit;

import python.lib.Types.KwArgs;
import sys.io.File;
import sys.io.Process;

private typedef T = {
	var value:Int;
	@:optional var maybeValue:Int;
}

private enum MyEnum {
	A(?x:Int, b:String);
	True;
	False;
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


	function testMakeVarArgs () {
		var f = function (a:Array<Dynamic>) {
			return a[0] + a[1];
		}
		var g = Reflect.makeVarArgs(f);
		var res = g(1,2);
		eq(3, res);
	}

	function testKwArgs () {
		function x (args:python.lib.Types.KwArgs) {
			var a = args.get("a", 0);
			var b = args.get("b", 0);
			return a + b;
		}


		var res = x( python.lib.Types.Dict.fromObject({ "a" : 1, "b" : 2}) );


		eq(3, res);

		var res2 = python.Syntax.callNamedUntyped(x, { a : 3, b : 5});

		eq(8, res2);
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
}