package unit.issues;

@:keep
private class CustomStringBuf extends StringBuf {
	public function new() {
		super();
	}

	override function add<T>(x:T) {
		super.add("add");
		super.add(x);
	}
}

class Issue6446 extends unit.Test {
	function test() {
		var buf = new CustomStringBuf();
		buf.add(" custom");
		eq("add custom", buf.toString());
	}
}