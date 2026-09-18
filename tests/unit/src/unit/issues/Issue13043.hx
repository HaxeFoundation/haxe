package unit.issues;

private class Base<T> {
	public var received : T;
	public function new() {}
	public dynamic function onItem(item : T) : Void {
		received = item;
	}
	public function trigger(item : T) {
		onItem(item);
	}
}

private class Sub extends Base<String> {
	override function onItem(item : String) {
		received = item.toUpperCase();
	}
}

class Issue13043 extends Test {
	function test() {
		var s = new Sub();
		s.trigger("hi");
		eq("HI", s.received);
	}
}
