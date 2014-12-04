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
	@:js('3;')
	static function testNoOpRemoval() {
		1;
		2;
		{}
		3;
	}

	@:js('
		27;
	')
	static function testConstMath() {
		var a = 1 + 2;
		var b = 9 * 3;
	}

	@:js('
		"foo";
	')
	static function testInlineCtor1() {
		var c = new InlineCtor(12, "foo");
		var x = c.x;
		c.x = 13;
		x = c.x;
		var y = c.y;
	}

	@:js('
		12;
	')
	static function testInlineCtor2() {
		var a = 0;
		var c = {
			a = 1;
			a = 2;
			new InlineCtor(12, "foo");
		}
		a = c.x;
	}

	@:js('
		1;
	')
	static function testInlineCtor3() {
		var a = 0;
		var b = {
			var c = new InlineCtor(1, "c");
			a = 1;
			new InlineCtor(2, "b");
		}
		b.x = a;
	}

	@:js('
		2;
	')
	static function testStructureInline1() {
		var x = {
			foo: 1,
			bar: 2
		}
		var y = x.foo;
		var z = x.bar;
	}

	@:js('
		"god";
	')
	static function testStructureInlineInvalidField() {
        var x = {
            "oh-my": "god"
        };
	}

	@:js('
		2;
	')
	static function testArrayInline() {
		var a = [1, 2];
		var b = a.length;
	}

	@:js('
		var a = [1,2];
		a[-1];
	')
	static function testArrayInlineCancelNegative() {
		var a = [1, 2];
		a[-1];
	}

	@:js('
		var a = [1,2];
		a[2];
	')
	static function testArrayInlineCancelExceeds() {
		var a = [1, 2];
		a[2];
	}

	@:js('
		"" + "a";
	')
	static function testAbstractOverStringBinop() {
		var s = "" + A;
	}
}