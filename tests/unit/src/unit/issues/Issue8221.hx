package unit.issues;

private abstract A(Dynamic) {
	public function iterator():Iterator<String> {
		var done = false;
		return {
			hasNext: () -> !done,
			next: () -> {done = true; "works";}
		};
	}
	@:op(a.b) function resolve(name:String):A throw "nope";
}

class Issue8221 extends unit.Test {
	function test() {
		var a:A = null;
		aeq(["works"], [for (v in a) v]);
	}
}
