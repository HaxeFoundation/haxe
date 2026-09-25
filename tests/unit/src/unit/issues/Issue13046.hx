package unit.issues;

private class Base<T> {
	public function new() {}
	public function call() : T {
		return null;
	}
	public function call2() : T {
		return null;
	}
}

private class Sub extends Base<Void> {
	public var calls = 0;
	override function call() : Void {
		calls++;
		if( calls > 1 )
			return; // force void return
		calls++;
	}
	override function call2() : Void {
		// empty body: no TReturn at all, ORet was missing
	}
}

class Issue13046 extends Test {

	function testMidReturn() {
		var sub = new Sub();
		var i : Base<Void> = sub;
		i.call();
		eq(2, sub.calls);
		i.call();
		eq(3, sub.calls);
		sub.call();
		eq(4, sub.calls);
	}

	function testEmptyBody() {
		var sub = new Sub();
		var i : Base<Void> = sub;
		i.call2();
		sub.call2();
		noAssert();
	}
}
