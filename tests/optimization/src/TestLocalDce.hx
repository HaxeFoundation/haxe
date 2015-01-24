private class InlineCtor {
	public var x:Int;
	public var y:String;

	public inline function new(x, y) {
		this.x = x;
		this.y = y;
	}
}

@:enum
private abstract MyEnum(String) to String {
	var A = "a";
}

@:analyzer(no_check_has_effect)
class TestLocalDce {
	@:js('console.log(3);')
	static function testNoOpRemoval() {
		1;
		2;
		{}
		trace(3);
	}

	@:js('
		console.log(27);
	')
	static function testConstMath() {
		var a = 1 + 2;
		var b = 9 * 3;
		trace(b);
	}

	@:js('
		console.log("foo");
	')
	static function testInlineCtor1() {
		var c = new InlineCtor(12, "foo");
		var x = c.x;
		c.x = 13;
		x = c.x;
		var y = c.y;
		trace(y);
	}

	@:js('
		console.log(12);
	')
	static function testInlineCtor2() {
		var a = 0;
		var c = {
			a = 1;
			a = 2;
			new InlineCtor(12, "foo");
		}
		trace(a = c.x);
	}

	@:js('
		console.log(1);
	')
	static function testInlineCtor3() {
		var a = 0;
		var b = {
			var c = new InlineCtor(1, "c");
			a = 1;
			new InlineCtor(2, "b");
		}
		trace(b.x = a);
	}

	@:js('
		console.log(2);
	')
	static function testStructureInline1() {
		var x = {
			foo: 1,
			bar: 2
		}
		var y = x.foo;
		var z = x.bar;
		trace(z);
	}

	@:js('
		TestLocalDce.keep("god");
	')
	static function testStructureInlineInvalidField() {
        var x = {
            "oh-my": keep("god")
        };
	}

	@:js('
		console.log(2);
	')
	static function testArrayInline() {
		var a = [1, 2];
		var b = a.length;
		trace(b);
	}

	@:js('
		var a = [1,2];
		console.log(a[-1]);
	')
	static function testArrayInlineCancelNegative() {
		var a = [1, 2];
		trace(a[-1]);
	}

	@:js('
		var a = [1,2];
		console.log(a[2]);
	')
	static function testArrayInlineCancelExceeds() {
		var a = [1, 2];
		trace(a[2]);
	}

	@:js('
		var s = "" + "a";
		console.log(s);
	')
	static function testAbstractOverStringBinop() {
		var s = "" + A;
		trace(s);
	}

	//@:js('
		//var s = TestLocalDce.keep(1);
		//s += 0;
		//s += 6;
		//s += 8;
		//console.log(s);
	//')
	static function testLoopUnroll() {
		var s = keep(1);
		for (i in [0, 3, 4]) {
			s += i * 2;
		}
		trace(s);
	}

	//@:js('console.log(5.);')
	static function testLoopUnrollDavid() {
		var s = 0.0;
		inline function foo(r)
			return 2.0 + r;
		for ( r in [0.0,1.0] )
			s+=foo(r);
		trace(s);
	}

	//@:js('
		//var s = TestLocalDce.keep(1);
		//var _g = 0;
		//var _g1 = [0,3,4];
		//while(_g < _g1.length) {
			//var i = _g1[_g];
			//++_g;
			//s += i * 2;
			//continue;
		//}
		//console.log(s);
	//')
	static function testLoopUnrollContinue() {
		var s = keep(1);
		for (i in [0, 3, 4]) {
			s += i * 2;
			continue;
		}
		trace(s);
	}

	//@:js('
		//var s = TestLocalDce.keep(1);
		//var _g = 0;
		//var _g1 = [0,3,4];
		//while(0 < _g1.length) {
			//var i = _g1[0];
			//++_g;
			//s += i * 2;
			//break;
		//}
		//console.log(s);
	//')
	static function testLoopUnrollBreak() {
		var s = keep(1);
		for (i in [0, 3, 4]) {
			s += i * 2;
			break;
		}
		trace(s);
	}

	static function keep(v:Dynamic) { return v; }
}