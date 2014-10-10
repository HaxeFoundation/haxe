class InlineCtor {
	public var x:Int;
	public var y:String;

	public inline function new(x, y) {
		this.x = x;
		this.y = y;
	}
}

@:enum abstract MyEnum(String) to String {
	var A = "a";
}

@:analyzer(no_local_dce)
@:analyzer(no_check_has_effect)
class Test {
	@:js('3;')
	static function testNoOpRemoval() {
		1;
		2;
		{}
		3;
	}

	@:js('
		var a = 3;
		var b = 27;
	')
	static function testConstMath() {
		var a = 1 + 2;
		var b = 9 * 3;
	}

	@:js('
		var c_x = 12;
		var c_y = "foo";
		var x = 12;
		c_x = 13;
		x = 13;
		var y = "foo";
	')
	static function testInlineCtor1() {
		var c = new InlineCtor(12, "foo");
		var x = c.x;
		c.x = 13;
		x = c.x;
		var y = c.y;
	}

	@:js('
		var a = 0;
		a = 1;
		a = 2;
		var c_x = 12;
		var c_y = "foo";
		a = 12;
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
		var a = 0;
		var c_x = 1;
		var c_y = "c";
		a = 1;
		var b_x = 2;
		var b_y = "b";
		b_x = 1;
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
		var x_foo = 1;
		var x_bar = 2;
		var y = 1;
		var z = 2;
	')
	static function testStructureInline1() {
		var x = {
			foo: 1,
			bar: 2
		}
		var y = x.foo;
		var z = x.bar;
	}

	@:js('var x = { \'oh-my\' : "god"};')
	static function testStructureInlineInvalidField() {
        var x = {
            "oh-my": "god"
        };
	}

	@:js('
		var a_0 = 1;
		var a_1 = 2;
		var b = 2;
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
		var s = "" + "a";
	')
	static function testAbstractOverStringBinop() {
		var s = "" + A;
	}

	@:js('
		var a = true;
		var b = 0;
		b = 1;
		b;
	')
	static function testSwitch1() {
		var a = true;
		var b = 0;
		switch (a) {
			case true: b = 1;
			case false: b = 2;
		}
		b; // TODO: this should become 1
	}

	@:js('
		var a = true;
		var b = 0;
		a = true;
		a;
	')
	static function testSwitch2() {
		var a = true;
		var b = 0;
		switch (b) {
			case -1: a = false;
			default: a = true;
		}
		a;
	}
}