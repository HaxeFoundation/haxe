package unit.issues;

class Issue3531 extends Test {
	function test() {
		eq("ab", 'a${}b');
	}
}