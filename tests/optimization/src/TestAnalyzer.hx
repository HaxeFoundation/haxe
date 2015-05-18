private enum E {
	A(s:String, i:Int, a:Array<Int>);
	B(?i:Int);
}

@:forward
private abstract Vec2({x:Float, y:Float}) from {x:Float, y:Float} {
	@:commutative @:op(A * B) static inline function mul(a:Vec2, b:Float):Vec2
		return {x: a.x * b, y: a.y * b};
}

class TestAnalyzer extends TestBase {

	static function main() {
		new TestAnalyzer();
	}

	public function new() {
		super();
		TestBaseMacro.run();
	}

	var buf = "";

	inline function append(s:String) {
		buf += s;
	}

	override function setup() {
		buf = "";
	}

	function test1() {
		var test = {
			append("1");
			append("2");
			var k = {
				append("3");
				99;
			}
			k;
		}
		test = {
			append("4");
			3;
		}
		assertEquals("1234", buf.toString());
	}

	function test2() {
		var a = if (cond1()) {
			append("1");
			if (cond2(getInt())) {
				append("2");
			} else {
				append("3");
			}
			buf.toString();
		} else {
			append("4");
			buf.toString();
		}
		assertEquals("cond11getIntcond23", a);
	}

	function test3() {
		var a = switch(getInt()) {
			case 1:
				1;
			case _:
				2;
		}
		assertEquals(1, a);
		assertEquals("getInt", buf.toString());
	}

	function test4() {
		var a = if (if(cond1()) cond2(0) else false) {
			1;
		} else {
			2;
		}
		assertEquals(1, a);
		assertEquals("cond1cond2", buf.toString());
	}

	function testCond1() {
		var a;
		if (cond1()) {
			a = 1;
		} else {
			a = 2;
		}
		assertEquals(1, a);
	}

	function testCond2() {
		var a;
		if (cond1()) {
			a = 1;
		} else {
			a = 1;
		}
		assertEqualsConst(1, a);
	}

	//function testUnop() {
		//var a = 0;
		//assertEqualsConst(0, a++);
		//assertEqualsConst(1, a);
		//assertEqualsConst(2, ++a);
		//assertEqualsConst(2, a);
		//if (cond1()) {
			//a++;
		//}
		//assertEquals(3, a);
	//}

	//function testMultiAssign() {
		//var a;
		//var b;
		//var c = a = b = 1;
		//assertEqualsConst(1, a);
		//assertEqualsConst(1, b);
		//assertEqualsConst(1, c);
		//c = a = b = 2;
		//assertEqualsConst(2, a);
		//assertEqualsConst(2, b);
		//assertEqualsConst(2, c);
	//}

	function testConst1() {
		var a = 1;
		a = 2;
		var b = a;
		assertEqualsConst(2, b);
		//b++;
		//assertEqualsConst(3, b);
	}

	function testConst2() {
		var a = 1;
		if (cond1()) {
			a = 2;
		} else {
			a = 3;
		}
		assertEquals(2, a);
	}

	function testConst3() {
		var a = 1;
		if (cond1()) {
			a = 2;
		} else {
			a = 2;
		}
		assertEqualsConst(2, a);
	}

	function testConst4() {
		var a = 1;
		if (cond1()) {
			a = 2;
		}
		assertEquals(2, a);
	}

	function testConst5() {
		var a = 2;
		if (cond1()) {
			a = 2;
		}
		assertEqualsConst(2, a);
	}

	function testConst6() {
		var a = 2;
		if (cond1()) {
			a = 2;
		} else {

		}
		assertEqualsConst(2, a);
	}

	function testConst7() {
		var a = 2;
		if (cond1()) {

		} else {
			a = 2;
		}
		assertEqualsConst(2, a);
	}

	function testConst8() {
		var a = 1;
		if (cond1()) {
			a = 2;
		} else {

		}
		assertEquals(2, a);
	}

	function testConst9() {
		var a = 1;
		var b = 2;
		if (cond1()) {

		} else {
			a = 2;
			b = 3;
		}
		assertEquals(1, a);
		assertEquals(2, b);
	}

	function testConst10() {
		var a = 1;
		var b = 2;
		if (cond2(1)) {
			a = 1;
		} else if (cond1()) {
			a = 1;
			b = 3;
		} else {
			a = 1;
		}
		assertEqualsConst(1, a);
		assertEquals(3, b);
	}

	function testConst11() {
		var a = 1;
		if (cond2(0)) {
			if (cond2(0)) {
				a = 2;
			} else {
				assertEqualsConst(1, a);
			}
		}
		assertEquals(2, a);
	}

	function testSwitch1() {
		var a = 1;
		switch (getBool()) {
			case true: a = 1;
			case false:
		}
		assertEqualsConst(1, a);
	}

	function testSwitch2() {
		var a = 1;
		switch (getBool()) {
			case true:
			case false: a = 2;
		}
		assertEquals(1, a);
	}

	function testSwitch3() {
		var a = 1;
		switch (getBool()) {
			case true: a = 2;
			case false: a = 2;
		}
		assertEqualsConst(2, a);
	}

	function testSwitch4() {
		var a = 1;
		switch (getInt()) {
			case 1: a = 1;
		}
		assertEqualsConst(1, a);
	}

	function testSwitch5() {
		var a = 1;
		switch (getInt()) {
			case 1: a = 1;
			case 2: a = 1;
		}
		assertEqualsConst(1, a);
	}

	function testSwitch6() {
		var a = 1;
		switch (getInt()) {
			case 1: a = 1;
			case 2: a = 2;
		}
		assertEquals(1, a);
	}

	function testSwitch7() {
		var a = 1;
		switch (getInt()) {
			case 1: a = 2;
			default: a = 2;
		}
		assertEqualsConst(2, a);
	}

	function testComplex1() {
		var a = 1;
		if (cond1()) {
			if (cond2(0)) {
				assertEqualsConst(1, a);
				a = 2;
				assertEqualsConst(2, a);
			} else {
				switch (getBool()) {
					case true:
						a = 2;
					case false:
						if (getInt() == -1) {
							a = 2;
						}
						assertEquals(1, a);
						a = 2;
				}
			}
			assertEqualsConst(2, a);
		}
		assertEquals(2, a);
	}

	function testLoop1() {
		var a = 1;
		var r = 0;
		while (true) {
			r = a;
			if (a > 1) {
				break;
			}
			a = 2;
		}
		assertEquals(2, a);
		assertEquals(2, r);
	}

	function testLoop2() {
		var a = 1;
		while (true) {
			a = 1;
			if (a == 1) {
				break;
			}
			a = 1;
		}
		assertEqualsConst(1, a);
	}

	function testLoop3() {
		var a = 1;
		while (cond1()) {
			a = 2;
			if (getInt() == 1) {
				break;
			}
			a = 1;
		}
		assertEquals(2, a);
	}

	function testLoop4() {
		var a = 1;
		while (true) {
			a = 2;
			a = 1;
			if (getInt() == 1) {
				break;
			}
			a = 1;
		}
		assertEqualsConst(1, a);
	}

	function testLoop5() {
		var a = 1;
		while (true) {
			assertEqualsConst(1, a);
			a = 2;
			if (getInt() == 1) {
				break;
			}
			a = 1;
		}
	}

	function testLoop6() {
		var a = 1;
		while (true) {
			assertEquals(1, a);
			a = 2;
			if (getInt() != 1) {
				continue;
			} else {
				break;
			}
		}
	}

	function testLoop7() {
		var a = 1;
		while (cond1()) {
			a = 2;
			assertEqualsConst(2, a);
			while (true) {
				a = 3;
				assertEqualsConst(3, a);
				a = 2;
				break;
			}
			assertEqualsConst(2, a);
			break;
		}
		assertEquals(2, a);
	}

	function testLoop8() {
		var a = 1;
		while (true) {
			assertEquals(1, a);
			if (getBool()) {
				break;
			}
			a = 2;
		}
		assertEquals(1, a);
	}

	function testTry1() {
		var a = 1;
		try {

		} catch(e:Dynamic) {
			assertEqualsConst(1, a);
			a = 2;
			assertEqualsConst(2, a);
		}
		// we do not check for unreachable catch cases at the moment
		assertEquals(1, a);
	}

	function testTry2() {
		var a = 1;
		try {
			assertEqualsConst(1, a);
			throw true;
		} catch(e:Dynamic) {
			assertEqualsConst(1, a);
			a = 2;
			assertEqualsConst(2, a);
		}
		assertEqualsConst(2, a);
	}

	function testTry3() {
		var a = 1;
		try {
			assertEqualsConst(1, a);
			throws();
			assertEqualsConst(1, a);
		} catch(e:Dynamic) {
			assertEqualsConst(1, a);
			a = 2;
			assertEqualsConst(2, a);
		}
		assertEquals(2, a);
	}

	function testTry4() {
		var a = 1;
		try {
			assertEqualsConst(1, a);
			a = 2;
			assertEqualsConst(2, a);
			throws();
			assertEqualsConst(2, a);
			a = 1;
			assertEqualsConst(1, a);
		} catch(e:Dynamic) {
			a = 1;
		}
		assertEqualsConst(1, a);
	}

	function testTry5() {
		var a = 1;
		try {
			throws();
			a = 2;
			a = 1;
		} catch (e:Dynamic) {

		}
		assertEqualsConst(1, a);
	}

	function testTry6() {
		var a = 1;
		try {
			a = 2;
			throws();
			a = 1;
			a = 2;
		} catch (e:String) {

		} catch (e:Dynamic) {

		}
		assertEqualsConst(2, a);
	}

	function testTry7() {
		var a = 1;
		try {
			throws();
			a = 2;
		} catch (e:String) {
			a = 2;
		} catch (e:Dynamic) {
			a = 2;
		}
		assertEqualsConst(2, a);
	}

	function testTry8() {
		var a = 1;
		try {
			if (cond1()) {
				a = 2;
				throw "out";
			}
		} catch(e:Dynamic) {

		}
		assertEquals(2, a);
	}

	function testTry9() {
		var a = 1;
		try {
			throw "foo";
		} catch(e:String) {
			a = 2;
		} catch(e:Dynamic) {
			a = 2;
		}
		assertEqualsConst(2, a);
	}

	function testTry10() {
		var a = 1;
		try {
			if (cond1()) {
				throw "foo";
			}
		} catch(e:String) {
			a = 2;
		} catch(e:Dynamic) {
			a = 2;
		}
		assertEquals(2, a);
	}

	function testTry11() {
		var a = 1;
		try {
			if (cond1()) {
				throw "foo";
			}
		} catch(e:String) {

		} catch(e:Dynamic) {
			a = 2;
		}
		assertEquals(1, a);
	}

	function testThrow1() {
		var a = 1;
		if (!cond1()) {
			a = 2;
			throw true;
		}
		assertEqualsConst(1, a);
	}

	function testThrow2() {
		var a = 1;
		if (!cond1()) {
			a = 2;
			if (!cond1()) {
				throw true;
			}
		}
		assertEquals(1, a);
	}

	function testReturn1() {
		var a = 1;
		if (!cond1()) {
			a = 2;
			return;
		}
		assertEqualsConst(1, a);
	}

	function testBreak1() {
		var a = 1;
		while (cond1()) {
			if (cond1()) {
				a = 2;
				break;
			}
			assertEqualsConst(1, a);
		}
		assertEquals(2, a);
	}

	function testContinue1() {
		var a = 1;
		while (true) {
			if (a == 2) {
				break;
			}
			if (cond1()) {
				a = 2;
				continue;
			}
			assertEquals(2, a);
		}
		assertEquals(2, a);
	}

	function testContinue2() {
		var a = 1;
		while (true) {
			if (a == 4) {
				break;
			}
			if (a > 3) {
				a = 4;
				continue;
			}
			++a;
		}
		assertEquals(4, a);
	}

	//function testMisc() {
		//var a = 1;
		//function call(a, b, c) { return a + b + c; }
		//assertEquals(5, call(assertEqualsConst(1, a), assertEqualsConst(2, a = a + 1), assertEqualsConst(2, a)));
		//assertEquals(22, call(assertEqualsConst(2, a++), assertEqualsConst(4, ++a), assertEqualsConst(16, a *= a)));
		//assertEquals(50, call(a, a = a + 1, a));
	//}

	function testEnumValues() {
		var array = [1];
		var a = A("foo", 12, array);
		switch (a) {
			case A(s, i, a):
				assertEqualsConst("foo", s);
				assertEqualsConst(12, i);
				assertEquals(array, a);
			case B(_):
		}

		var b = B(0);
		switch (b) {
			case B(i):
				assertEqualsConst(0, i);
			case A(_):
		}
	}

	function testIssue3869() {
        var v1:Vec2 = {x: 1., y: 2.};
        var r = v1 * 2;
		assertEqualsConst(2., r.x);
		assertEqualsConst(4., r.y);

        var v2:Vec2 = {x: 1., y: 2.};
        var r2 = 2 * v2;
		assertEqualsConst(2., r2.x);
		assertEqualsConst(4., r2.y);
	}

	function testApiInline() {
		var i = 65;
		var f = 3.5;
		var s = "foo";
		var bTrue = true;
		var bFalse = false;
		var eBreak = haxe.macro.Expr.ExprDef.EBreak;

		var floor = Math.floor(f);
		assertEqualsConst(3, floor);

		var ceil = Math.ceil(f);
		assertEqualsConst(4, ceil);

		var int = Std.int(f);
		assertEqualsConst(3, int);

		var string = Std.string(s);
		assertEqualsConst("foo", string);

		var string = Std.string(bTrue);
		assertEqualsConst("true", string);

		var string = Std.string(bFalse);
		assertEqualsConst("false", string);

		var fromCharCode = String.fromCharCode(i);
		assertEqualsConst("A", fromCharCode);

		var enumIndex = Type.enumIndex(eBreak);
		assertEqualsConst(20, enumIndex);
	}

	function cond1() {
		append("cond1");
		return true;
	}

	function cond2(i) {
		append("cond2");
		return i == 0;
	}

	function getInt() {
		append("getInt");
		return 1;
	}

	function getBool() {
		return true;
	}

	function throws() {
		throw true;
	}

	function assertEqualsConst<T>(expected:T, actual:T, ?p) {
		assertEquals(expected, actual, p);
		return actual;
	}
}