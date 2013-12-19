class InlineCtor {
	public var x:Int;
	public var y:String;
	
	public inline function new(x, y) {
		this.x = x;
		this.y = y;
	}
}

class Test {
	@:js("3;")
	static function testNoOpRemoval() {
		1;
		2;
		{}
		3;
	}
	
	@:js("var a = 3;var b = 27;")
	static function testConstMath() {
		var a = 1 + 2;
		var b = 9 * 3;
	}
	
	@:js("var c_x = 12;var c_y = \"foo\";var x = c_x;c_x = 13;x = c_x;var y = c_y;")
	static function testInlineCtor1() {
		var c = new InlineCtor(12, "foo");
		var x = c.x;
		c.x = 13;
		x = c.x;
		var y = c.y;
	}
	
	@:js("var a = 0;a = 1;a = 2;var c_x = 12;var c_y = \"foo\";a = c_x;")
	static function testInlineCtor2() {
		var a = 0;
		var c = {
			a = 1;
			a = 2;
			new InlineCtor(12, "foo");
		}
		a = c.x;
	}
	
	@:js("var a = 0;var c_x = 1;var c_y = \"c\";a = 1;var b_x = 2;var b_y = \"b\";b_x = a;")
	static function testInlineCtor3() {
		var a = 0;
		var b = {
			var c = new InlineCtor(1, "c");
			a = 1;
			new InlineCtor(2, "b");
		}
		b.x = a;
	}

	@:js("var x = 10;\"\" + x;var x1 = 10;\"\" + x1;var x2 = 10.0;\"\" + x2;var x3 = \"10\";x3;var x4 = true;\"\" + x4;")
	static function testStdString() {
        var x = 10;
        Std.string(x);
        var x:UInt = 10;
        Std.string(x);
        var x = 10.0;
        Std.string(x);
        var x = "10";
        Std.string(x);
        var x = true;
        Std.string(x);
	}
}