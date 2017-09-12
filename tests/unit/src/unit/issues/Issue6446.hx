package unit.issues;

@:keep
private class CustomStringBuf extends StringBuf {
	public function new() {
		super();
	}

	#if eval
	override function add<T>(x:T) {
		super.add("add");
		super.add(x);
	}
	#end
}

class Issue6446 extends unit.Test {
	function test() {
		var buf = new CustomStringBuf();
		buf.add(" custom");
		eq(#if eval "add" #else "" #end + " custom", buf.toString());
	}
}