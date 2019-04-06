package unit.issues;

class Issue5851 extends unit.Test {
	function test() {
		noAssert();
		try {
			return "";
		} catch ( s : Dynamic ) {
			return "";
		}
		return "";
	}
}