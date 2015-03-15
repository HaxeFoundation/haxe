package unit.issues;

class Issue2917 extends Test {
	function test() {
		var m = new Map<String, Int>();
		eq(0, Lambda.count(m));
		m.set("foo", 12);
		eq(1, Lambda.count(m));
	}
}