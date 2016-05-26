package unit.issues;

private class MyTest<T> {
	var v:T;
	public function new(v:T):Void {
		this.v = v;
	}
	public function a():T
		return v;

	inline public function test():T
		return a();
}

class Issue4384 extends Test {
	function test() {
		var t = new MyTest([123]);
		eq(123, t.test()[0]);
	}
}