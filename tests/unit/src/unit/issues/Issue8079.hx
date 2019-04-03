package unit.issues;

class Issue8079 extends unit.Test {
	function test() {
		var instance = ({}:Dynamic).instance;
		if(instance.timeout != null) {
			instance.timeout = null;
		}
	}
}