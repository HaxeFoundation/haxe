package unit.issues;

class Issue6562 extends unit.Test {

	static inline function inlineTry(i:Int) {
		return {
			try {
				"x";
			} catch (e:Dynamic) {
				trace(e);
			}
			"hey";
		}
	}

	function test() {
		inlineTry(1);
	}
}