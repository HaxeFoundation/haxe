package unit.issues;

class Issue6724 extends unit.Test {
	function test() {
		var v = switch (null) {
			case v = null: v;
			case _: throw false;
		}
		eq(null, v);
	}
}