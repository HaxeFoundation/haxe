package unit.issues;

class Issue10193 extends Test {
	function test() {
		var c = new C(42);
		eq(42, c.a);
		eq(42, c.f());
	}
}

@:keep
@:native("Issue10193E")
private class EImpl {
	public var a:Int;
	public function new(a:Int) {
		this.a = a;
	}
}

@:native("Issue10193E")
private extern class E {
	var a:Int;
	function new(a:Int);
}

private class C extends E {
	public var f:()->Int;
	public function new(a:Int) {
		super(a);
		this.f = () -> this.a;
	}
}
