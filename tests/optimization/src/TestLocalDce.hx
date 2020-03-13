import TestJs.use;

private class InlineCtor {
	public var x:Int;
	public var y:String;

	public inline function new(x, y) {
		this.x = x;
		this.y = y;
	}
}

private enum abstract MyEnum(String) to String {
	var A = "a";
}

@:analyzer(no_user_var_fusion)
class TestLocalDce {
	@:js('TestJs.use(3);')
	static function testNoOpRemoval() {
		1;
		2;
		{}
		use(3);
	}

	@:js('
		TestJs.use(27);
	')
	static function testConstMath() {
		var a = 1 + 2;
		var b = 9 * 3;
		use(b);
	}

	@:js('
		TestJs.use("foo");
	')
	static function testInlineCtor1() {
		var c = new InlineCtor(12, "foo");
		var x = c.x;
		c.x = 13;
		x = c.x;
		var y = c.y;
		use(y);
	}

	@:js('
		TestJs.use(12);
	')
	static function testInlineCtor2() {
		var a = 0;
		var c = {
			a = 1;
			a = 2;
			new InlineCtor(12, "foo");
		}
		use(a = c.x);
	}

	@:js('
		TestJs.use(1);
	')
	static function testInlineCtor3() {
		var a = 0;
		var b = {
			var c = new InlineCtor(1, "c");
			a = 1;
			new InlineCtor(2, "b");
		}
		use(b.x = a);
	}

	@:js('
		TestJs.use(2);
	')
	static function testStructureInline1() {
		var x = {
			foo: 1,
			bar: 2
		}
		var y = x.foo;
		var z = x.bar;
		use(z);
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
		TestJs.use(2);
	')
	static function testArrayInline() {
		var a = [1, 2];
		var b = a.length;
		use(b);
	}

	@:js('
		var a = [1,2];
		TestJs.use(a[-1]);
	')
	static function testArrayInlineCancelNegative() {
		var a = [1, 2];
		use(a[-1]);
	}

	@:js('
		var a = [1,2];
		TestJs.use(a[2]);
	')
	static function testArrayInlineCancelExceeds() {
		var a = [1, 2];
		use(a[2]);
	}

	@:js('
		TestJs.use("a");
	')
	static function testAbstractOverStringBinop() {
		var s = "" + A;
		use(s);
	}

	@:js('
		var s = TestLocalDce.keep(1);
		s += 0;
		s += 6;
		s += 8;
		TestJs.use(s);
	')
	static function testLoopUnroll() {
		var s = keep(1);
		for (i in [0, 3, 4]) {
			s += i * 2;
		}
		use(s);
	}

	@:js('TestJs.use(5.);')
	static function testLoopUnrollDavid() {
		var s = 0.0;
		inline function foo(r)
			return 2.0 + r;
		for ( r in [0.0,1.0] )
			s+=foo(r);
		use(s);
	}

	@:js('
		var s = TestLocalDce.keep(1);
		var _g = 0;
		var _g1 = [0,3,4];
		while(_g < _g1.length) {
			var i = _g1[_g];
			++_g;
			s += i * 2;
		}
		TestJs.use(s);
	')
	static function testLoopUnrollContinue() {
		var s = keep(1);
		for (i in [0, 3, 4]) {
			s += i * 2;
			continue;
		}
		use(s);
	}

	@:js('
		var s = TestLocalDce.keep(1);
		var _g1 = [0,3,4];
		while(0 < _g1.length) {
			var i = _g1[0];
			s += i * 2;
			break;
		}
		TestJs.use(s);
	')
	static function testLoopUnrollBreak() {
		var s = keep(1);
		for (i in [0, 3, 4]) {
			s += i * 2;
			break;
		}
		use(s);
	}

	@:pure(false)
	static function keep(v:Dynamic) { return v; }
}