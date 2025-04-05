package unit.issues;

class Issue10960 extends Test {
	function test() {
		final obj:Dynamic = 0;
		eq("", mm(obj));
	}

	static macro function mm(any:Any) {
		return macro "";
	}
}
