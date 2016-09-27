package unit.issues;

@:generic
private class G<T> {
	var t:T;
	public function new(t:T) {
		this.t = t;
	}
	public function f(){
		haxe.Log;
		return t;
	}
}

private class G2 extends G<String> { }

private class G1 extends G<Int> { }

class Issue2902 extends Test {
	function test() {
		var g1 = new G1(12);
		var g2 = new G2("foo");
		eq(12, g1.f());
		eq("foo", g2.f());
	}
}