package unit.issues;

class Issue8079 extends unit.Test {
	function test() {
		var instance = ({}:Dynamic);
		if(instance.timeout != null) {
			instance.timeout = null;
		}
		utest.Assert.pass();
	}
}