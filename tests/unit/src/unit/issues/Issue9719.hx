package unit.issues;

private abstract class Parent<K> {
	public function new() {}
	public abstract function put(k:K):Bool;
}

private class Child<K> extends Parent<K> {
	public override function put(k:K) {
		return true;
	}
}

private class Child2<K> extends Parent<String> {
	public override function put(k:String) {
		return true;
	}
}

class Issue9719 extends unit.Test {
	function test() {
		var child = new Child();
		t(child.put(null));
		var parent:Parent<String> = child;
		t(parent.put(null));

		var child2 = new Child2();
		t(child2.put("foo"));
	}
}