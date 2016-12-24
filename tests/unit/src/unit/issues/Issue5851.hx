package unit.issues;

class Issue5851 extends unit.Test {
	function test() {
		try {
			return "";
		} catch ( s : Dynamic ) {
			return "";
		}
		return "";
	}
}