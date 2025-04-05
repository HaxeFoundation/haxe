package unit.issues;

class Issue9491 extends unit.Test {
	function test() {
		var first = (arg) -> {
			return "";
		};

		var second = (firstArg : String, secondArg = 0) -> {
			return first(secondArg);
		};
		eq("", second(""));
	}
}