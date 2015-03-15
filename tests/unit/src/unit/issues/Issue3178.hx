package unit.issues;

private abstract A(String) {
	public inline function new(s : String) {
		this = s;
	}

	public function f(index : Int) {
		return StringTools.fastCodeAt(this, index);
	}

	public inline function g(index : Int) {
		return StringTools.fastCodeAt(this, index);
	}
}

class Issue3178 extends unit.Test {
	function test() {
		var a = new A("a");
		eq(97, a.f(0));
		eq(97, a.g(0));
	}
}