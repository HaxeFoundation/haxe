package unit.issues;

class Issue4593 extends Test {
	public function test() {
		var a:Foo = 1;
		var b = "value is " + a;
		eq("value is INT 1", b);
	}
}

abstract Foo(Int) from Int {
	@:to function print():String {
		return 'INT $this';
	}
}