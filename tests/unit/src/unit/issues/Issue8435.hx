package unit.issues;

class Issue8435 extends unit.Test {
	function test() {
		var data: Dynamic<()->String> = {test: () -> "test"};
		eq("test", data.test());
	}
}