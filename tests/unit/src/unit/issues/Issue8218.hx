package unit.issues;

class Issue8218 extends unit.Test {
	#if flash
	function test() {
		new Child(this);
	}
	#end
}

#if flash
@:access(unit.Test)
private class Child extends Lib2 {
	public function new(test:unit.Test) {
		super();

		// this. protected access
		test.eq(x, 42);
		x = 50;
		test.eq(x, 50);

		test.eq(i, 5);
		i = 10;
		test.eq(i, 10);

		test.eq(f(), "hello");
		test.eq(call(f), "hello");

		// static protected access
		test.eq(Lib.sx, 42);
		Lib.sx = 50;
		test.eq(Lib.sx, 50);

		test.eq(Lib.si, 5);
		Lib.si = 10;
		test.eq(Lib.si, 10);

		test.eq(Lib.sf(), "hello");
	}

	static function call(f:()->String):String {
		return f();
	}
}
#end